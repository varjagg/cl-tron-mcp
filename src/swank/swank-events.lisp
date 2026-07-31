;;;; src/swank/swank-events.lisp - Event queue, reconnection, event processor
;;;;
;;;; Handles:
;;;;   - swank-event struct (type, data, timestamp)
;;;;   - Enqueueing/dequeuing :debug and :output events
;;;;   - Automatic reconnection with exponential backoff
;;;;   - Background event processor thread
;;;;
;;;; Load order: loaded after swank-connection.lisp and swank-rpc.lisp

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Event Queue
;;; ============================================================
;;;
;;; Async events from Swank are queued for later processing:
;;;   - :debug  — debugger entered; retrieved by pop-debugger-event
;;;   - :output — text written by evaluated code

(defstruct swank-event
  type
  data
  timestamp)

(defun cleanup-old-events-unlocked (max-age maximum-size)
  "Replace *EVENT-QUEUE* with its newest live events.

The caller holds *EVENT-MUTEX*.  MAXIMUM-SIZE may be one less than the public
queue limit when an enqueue operation needs to reserve one slot."
  (let* ((current-time (get-unix-time))
         (live-events
           (loop for event across *event-queue*
                 unless (> (- current-time (swank-event-timestamp event))
                           max-age)
                   collect event))
         (kept-events (last live-events (max 0 maximum-size)))
         (new-queue (make-array (max 100 (length kept-events))
                                :adjustable t
                                :fill-pointer 0)))
    (dolist (event kept-events)
      (vector-push-extend event new-queue))
    (setf *event-queue* new-queue)))

(defun prepare-event-queue-for-enqueue ()
  "Expire old events and reserve one queue slot.  Caller holds the mutex."
  (when (>= (length *event-queue*) *max-event-queue-size*)
    (cleanup-old-events-unlocked 300 (1- *max-event-queue-size*))))

(defun enqueue-debugger-event
    (condition restarts frames
     &key
       (thread nil thread-p)
       (level nil level-p)
       expected-generation)
  "Enqueue a :debug event from Swank when its connection is still current."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (unless (and expected-generation
                 (/= expected-generation *connection-generation*))
      (when thread-p
        (setf *debugger-thread* thread)
        (incf *debugger-episode*))
      (when level-p
        (setf *debugger-level* level))
      (prepare-event-queue-for-enqueue)
      (vector-push-extend
       (make-swank-event :type :debug
                         :data (list :condition condition
                                     :restarts restarts
                                     :frames frames)
                         :timestamp (get-unix-time))
       *event-queue*)
      (bordeaux-threads:condition-notify *event-condition*)
      t)))

(defun enqueue-output-event (string target &optional expected-generation)
  "Enqueue an :output event when its connection is still current."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (unless (and expected-generation
                 (/= expected-generation *connection-generation*))
      (prepare-event-queue-for-enqueue)
      (vector-push-extend
       (make-swank-event :type :output
                         :data (list :string string :target target)
                         :timestamp (get-unix-time))
       *event-queue*)
      (bordeaux-threads:condition-notify *event-condition*)
      t)))

(defun note-debugger-activation (thread level &optional expected-generation)
  "Record an active debugger level when its connection is still current."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (unless (and expected-generation
                 (/= expected-generation *connection-generation*))
      (setf *debugger-thread* thread
            *debugger-level* level)
      t)))

(defun cleanup-old-events (&optional (max-age 300))
  "Remove events older than MAX-AGE seconds; also trim if queue is oversized."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (cleanup-old-events-unlocked max-age *max-event-queue-size*)))

(defun non-debug-event-index ()
  "Return the index of the oldest event handled by the background processor."
  (position-if-not (lambda (event)
                     (eq (swank-event-type event) :debug))
                   *event-queue*))

(defun remove-event-at-index (index)
  "Remove and return event INDEX from *EVENT-QUEUE*.  Caller holds the mutex."
  (let ((event (aref *event-queue* index)))
    (replace *event-queue* *event-queue*
             :start1 index
             :start2 (1+ index))
    (decf (fill-pointer *event-queue*))
    event))

(defun dequeue-event (&optional (timeout 0.1))
  "Dequeue and return the next non-debug event, or NIL on timeout.
Debug events remain in the queue for pop-debugger-event."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (let ((deadline (+ (get-internal-real-time)
                       (round (* timeout internal-time-units-per-second)))))
      (loop
        for index = (non-debug-event-index)
        when index
          return (remove-event-at-index index)
        unless *event-processor-running*
          return nil
        do (let ((remaining (/ (- deadline (get-internal-real-time))
                               (float internal-time-units-per-second))))
             (when (<= remaining 0)
               (return nil))
             ;; A queue containing only debugger events is not actionable by
             ;; this worker.  Wait just as we do for an empty queue instead of
             ;; polling it at full CPU while preserving those events for SLDB.
             (bordeaux-threads:condition-wait
              *event-condition* *event-mutex* :timeout remaining))))))

;;; ============================================================
;;; Reconnection (circuit breaker with exponential backoff)
;;; ============================================================

(defun attempt-reconnect (&key (host "127.0.0.1")
                               (port (cl-tron-mcp/config:get-config :swank-port))
                               expected-generation)
  "Attempt to reconnect to Swank with exponential backoff.
Returns connection status plist or error plist."
  (unless *reconnect-enabled*
    (return-from attempt-reconnect
      (cl-tron-mcp/resources:make-error "RECONNECTION_DISABLED")))
  (labels ((cancelled-p ()
             (and expected-generation
                  (bordeaux-threads:with-lock-held (*connection-lock*)
                    (/= expected-generation *connection-generation*))))
           (cancelled-result ()
             (list :cancelled t :message "Reconnection was superseded"))
           (wait-for-retry (delay)
             (let ((deadline
                     (+ (get-internal-real-time)
                        (round (* delay
                                  internal-time-units-per-second)))))
               (loop
                 (when (cancelled-p)
                   (return nil))
                 (let ((remaining
                         (/ (- deadline (get-internal-real-time))
                            (float internal-time-units-per-second))))
                   (when (<= remaining 0)
                     (return t))
                   (sleep (min remaining 0.25))))))
           (maximum-attempts-result ()
             (log-error
              (format nil "Max reconnection attempts (~d) reached"
                      *reconnect-max-attempts*))
             (bordeaux-threads:with-lock-held (*connection-lock*)
               (when (or (null expected-generation)
                         (= expected-generation *connection-generation*))
                 (setf *reconnect-attempt-count* 0)))
             (cl-tron-mcp/resources:make-error
              "MAX_RECONNECTION_ATTEMPTS"
              :details (list :max-attempts *reconnect-max-attempts*))))
    (when (cancelled-p)
      (return-from attempt-reconnect (cancelled-result)))
    (bordeaux-threads:with-lock-held (*connection-lock*)
      (when *swank-connected*
        (return-from attempt-reconnect
          (cl-tron-mcp/resources:make-error "SWANK_ALREADY_CONNECTED"))))
    (loop
      do (let ((attempt-count
                 ;; Check cancellation and consume a retry slot atomically.
                 ;; A successful manual connection resets the counter under
                 ;; this same lock, so a superseded worker cannot increment it
                 ;; again before noticing the new generation.
                 (bordeaux-threads:with-lock-held (*connection-lock*)
                   (unless (and expected-generation
                                (/= expected-generation
                                    *connection-generation*))
                     (incf *reconnect-attempt-count*)))))
           (unless attempt-count
             (return (cancelled-result)))
           (when (> attempt-count *reconnect-max-attempts*)
             (return (maximum-attempts-result)))
           (let ((delay (* *reconnect-delay* (expt 2 (1- attempt-count)))))
             (log-info
              (format nil "Reconnection attempt ~d/~d, waiting ~d seconds"
                      attempt-count *reconnect-max-attempts* delay))
             (unless (wait-for-retry delay)
               (return (cancelled-result))))
           (when (cancelled-p)
             (return (cancelled-result)))
           (multiple-value-bind (result retry-generation)
               (handler-case
                   (swank-connect :host host :port port
                                  :expected-generation expected-generation)
                 (error (e)
                   (log-error
                    (format nil "Reconnection attempt ~d error: ~a"
                            attempt-count e))
                   (cl-tron-mcp/resources:make-error
                    "RECONNECTION_ERROR"
                    :details (list :error (princ-to-string e)))))
             ;; A partial installation advances the generation to invalidate
             ;; any workers it managed to start.  Continue only if that exact
             ;; detached generation is still current; an outside transition
             ;; still supersedes this reconnect series.
             (when (and expected-generation retry-generation)
               (let ((adopted-p
                       (bordeaux-threads:with-lock-held (*connection-lock*)
                         (when (= retry-generation *connection-generation*)
                           (setf expected-generation retry-generation)
                           t))))
                 (unless adopted-p
                   (return (cancelled-result)))))
             (cond
               ((getf result :cancelled)
                (return result))
               ((getf result :error)
                (log-error
                 (format nil "Reconnection attempt ~d failed: ~a"
                         attempt-count (getf result :message)))
                (when (>= attempt-count *reconnect-max-attempts*)
                  (return (maximum-attempts-result))))
               (t
                (log-info
                 (format nil "Reconnection successful on attempt ~d"
                         attempt-count))
                (let ((connected-generation (getf result :generation)))
                  (bordeaux-threads:with-lock-held (*connection-lock*)
                    (when (or (and connected-generation
                                   (= connected-generation
                                      *connection-generation*))
                              (and (null connected-generation)
                                   (or (null expected-generation)
                                       (= expected-generation
                                          *connection-generation*))))
                      (setf *reconnect-attempt-count* 0))))
                (return result))))))))

(defun start-reconnect-worker (&key expected-generation host port)
  "Start one background reconnect worker for a detached connection."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (when (and expected-generation
               (/= expected-generation *connection-generation*))
      (return-from start-reconnect-worker
        (list :cancelled t :message "Reconnection was superseded")))
    (when (and *reconnect-thread*
               (eql *reconnect-generation* expected-generation)
               (bordeaux-threads:thread-alive-p *reconnect-thread*))
      (return-from start-reconnect-worker
        (list :already-running t)))
    (let (thread)
      ;; The new worker first takes *CONNECTION-LOCK* in ATTEMPT-RECONNECT,
      ;; so creating it while holding the lock makes publication race-free.
      (setf thread
            (cl-tron-mcp/logging:make-diagnostic-thread
             (lambda ()
               (unwind-protect
                    (apply #'attempt-reconnect
                           (append
                            (list :expected-generation expected-generation)
                            (when host (list :host host))
                            (when port (list :port port))))
                 (bordeaux-threads:with-lock-held (*connection-lock*)
                   (when (and
                          (eq *reconnect-thread*
                              (bordeaux-threads:current-thread))
                          (eql *reconnect-generation*
                               expected-generation))
                     (setf *reconnect-thread* nil
                           *reconnect-generation* nil)))))
             :name "swank-reconnect")
            *reconnect-thread* thread
            *reconnect-generation* expected-generation)
      (list :scheduled t))))

(defun cleanup-on-error ()
  "Clean up resources on fatal errors: disconnect, clear pending state."
  (handler-case
      (progn
        (log-warn "Cleaning up after fatal error")
        (swank-disconnect)
        (bordeaux-threads:with-lock-held (*request-lock*)
          (clrhash *pending-requests*)
          (setf *current-request-id* nil))
        (bordeaux-threads:with-lock-held (*event-mutex*)
          (setf (fill-pointer *event-queue*) 0))
        (setf *debugger-thread* nil
              *debugger-level* 0)
        (log-info "Cleanup completed"))
    (error (e)
      (log-error (format nil "Error during cleanup: ~a" e)))))

;;; ============================================================
;;; Event Processor Thread
;;; ============================================================

(defun swank-event-processor (&optional (generation *connection-generation*))
  "Background thread: consumes non-debug events from the queue."
  (log-debug "Swank event processor started")
  (loop while (and *event-processor-running*
                   (= generation *connection-generation*))
        do (let ((event (dequeue-event 1)))
             (when event
               (handle-swank-event event))))
  (log-debug "Swank event processor exiting"))

(defun handle-swank-event (event)
  "Process a single Swank event.
:debug events remain in the queue for pop-debugger-event; :output events are logged."
  (ecase (swank-event-type event)
    (:debug
     (log-info (format nil "Debugger event queued: ~a"
                       (getf (swank-event-data event) :condition))))
    (:output
     (let ((data (swank-event-data event)))
       (log-debug (format nil "Output: ~a" (getf data :string)))))))

;;; ============================================================
;;; Debugger Event API (used by MCP tools)
;;; ============================================================

(defun pop-debugger-event ()
  "Pop and return the most recent debug event as (values condition restarts frames).
Returns NIL if no debug event is queued."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (loop for i from (1- (length *event-queue*)) downto 0
          when (eq (swank-event-type (aref *event-queue* i)) :debug)
            do (let* ((event (aref *event-queue* i))
                      (data (swank-event-data event)))
                 (setf *event-queue*
                       (delete (aref *event-queue* i) *event-queue* :test #'eq))
                 (return (values (getf data :condition)
                                 (getf data :restarts)
                                 (getf data :frames)))))))

(defun note-debugger-return
    (level &key expected-generation
                (expected-thread nil expected-thread-p)
                (expected-episode nil expected-episode-p))
  "Update cached debugger state after Swank reports a debugger return.
Removes the most recent cached :debug event. If LEVEL is the outermost
debugger level, clears debugger state entirely; otherwise decrements the
tracked nesting level."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    ;; SLDB commands can return before or after Swank's :DEBUG-RETURN event.
    ;; Apply a level transition once whichever path observes it first.
    (when (and (or (null expected-generation)
                   (= expected-generation *connection-generation*))
               (or (not expected-thread-p)
                   (equal expected-thread *debugger-thread*))
               (or (not expected-episode-p)
                   (= expected-episode *debugger-episode*))
               (>= *debugger-level* level))
      (loop for i from (1- (length *event-queue*)) downto 0
            when (eq (swank-event-type (aref *event-queue* i)) :debug)
              do (setf *event-queue*
                       (delete (aref *event-queue* i) *event-queue* :test #'eq))
                 (return))
      (if (<= level 1)
          (setf *debugger-thread* nil
                *debugger-level* 0)
          (setf *debugger-level* (1- level))))))
