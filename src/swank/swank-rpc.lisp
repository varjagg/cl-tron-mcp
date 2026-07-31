;;;; src/swank/swank-rpc.lisp - Request-response correlation, reader loop, dispatch
;;;;
;;;; Handles:
;;;;   - Request ID generation and the swank-request struct
;;;;   - Synchronous (wait-for-response) and asynchronous (send-request-async) RPC
;;;;   - Background reader thread (swank-reader-loop)
;;;;   - Heartbeat/keepalive thread (heartbeat-loop)
;;;;   - Incoming message dispatch (dispatch-incoming-message)
;;;;   - Output handling (handle-output)
;;;;
;;;; Load order: loaded after swank-connection.lisp

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Request-Response Correlation
;;; ============================================================
;;;
;;; Swank uses RPC with request IDs.
;;; Flow:
;;;   1. Client sends  (:emacs-rex form package thread id)
;;;   2. Server replies (:return (:ok result) id) or (:return (:abort) id)
;;;   3. Server may also send :debug, :write-string async events

(defun make-request-id ()
  (bordeaux-threads:with-lock-held (*request-lock*)
    (prog1 *next-request-id*
      (incf *next-request-id*))))

(defstruct swank-request
  id
  condition
  response
  completed-p)

(defun fail-pending-requests (message)
  "Wake every request waiting on a connection which has been detached."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (maphash
     (lambda (id request)
       (declare (ignore id))
       (unless (swank-request-completed-p request)
         (setf (swank-request-response request)
               (list :error t
                     :code "SWANK_CONNECTION_CLOSED"
                     :message message)
               (swank-request-completed-p request) t)
         (bordeaux-threads:condition-notify
          (swank-request-condition request))))
     *pending-requests*)
    (setf *current-request-id* nil)))

(defun register-request (request)
  "Register REQUEST against one specific live connection generation."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (when (and *swank-connected* *swank-running*
               *swank-io* *swank-socket*)
      (let ((generation *connection-generation*)
            (id (swank-request-id request)))
        (bordeaux-threads:with-lock-held (*request-lock*)
          (setf (gethash id *pending-requests*) request
                *current-request-id* id))
        (values t generation)))))

(defun fulfill-request (id response &optional expected-generation)
  "Mark request ID as completed when its connection is still current."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (and (or (null expected-generation)
                        (= expected-generation *connection-generation*))
                    (gethash id *pending-requests*))))
      (when req
        (setf (swank-request-response req) response
              (swank-request-completed-p req) t)
        (bordeaux-threads:condition-notify (swank-request-condition req))))))

(defun wait-for-response (id &key (timeout *default-eval-timeout*))
  "Wait up to TIMEOUT seconds for the response to request ID.

Polls the request's condition variable with a BOUNDED wait (at most one
second per iteration) so the deadline is always honoured.  The previous
version called CONDITION-WAIT with no timeout: if a rex was never answered
— e.g. a debugger command routed to a thread that never replies (FD-009
bug #9) — the wait blocked forever, the elapsed-time check never ran, and
because the MCP stdio loop handles requests serially that one blocked call
wedged the entire connection.  With a bounded wait an unanswered rex now
returns a clean timeout error instead."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (gethash id *pending-requests*)))
      (unless req
        (return-from wait-for-response
          (list :error t :message (format nil "Request ~a not found" id))))
      (let ((deadline (+ (get-unix-time) timeout)))
        (loop
          (when (swank-request-completed-p req)
            (return (swank-request-response req)))
          (let ((remaining (- deadline (get-unix-time))))
            (when (<= remaining 0)
              ;; Give up.  Stop tracking the request so a late reply cannot
              ;; notify a condition nobody waits on, and clear the current-id
              ;; pointer if it still refers to us — leaves clean state for
              ;; the next request rather than a permanently stuck connection.
              (remhash id *pending-requests*)
              (when (eql *current-request-id* id)
                (setf *current-request-id* nil))
              (return (list :error t :timeout t
                            :message (format nil "Request ~a timed out after ~a seconds"
                                             id timeout))))
            (bordeaux-threads:condition-wait
             (swank-request-condition req) *request-lock*
             :timeout (min remaining 1))))))))

;;; ============================================================
;;; Reader Thread & Message Dispatch
;;; ============================================================

(defun swank-reader-loop (&optional (generation *connection-generation*)
                                    (io *swank-io*)
                                    (socket *swank-socket*))
  "Background thread: continuously read incoming Swank messages."
  (log-debug "Swank reader thread started")
  (let ((connection-failed-p nil))
    (unwind-protect
         (loop while (and *swank-running*
                          (= generation *connection-generation*))
               do (handler-case
                      (let* ((raw-message
                               (cl-tron-mcp/swank-protocol:read-packet io))
                             (message
                               (handler-case
                                   (cl-tron-mcp/swank-protocol:read-form
                                    raw-message *swank-io-package*)
                                 (error (e)
                                   (log-error
                                    (format nil "Failed to parse message ~S: ~a"
                                            raw-message e))
                                   (setf connection-failed-p t)
                                   (return)))))
                        ;; A reader from an invalidated connection may finish one
                        ;; last read.  It must not dispatch that packet into the
                        ;; replacement connection's request table.
                        (when (/= generation *connection-generation*)
                          (return))
                        (log-debug (format nil "Received: ~s" message))
                        (bordeaux-threads:with-lock-held (*connection-lock*)
                          (when (= generation *connection-generation*)
                            (setf *last-activity-time* (get-unix-time))))
                        (when (= generation *connection-generation*)
                          (dispatch-incoming-message message generation)))
                    ;; I/O timeouts are non-fatal: the socket may have a
                    ;; residual SO_RCVTIMEO from usocket:socket-connect.
                    (cl-tron-mcp/swank-protocol:swank-read-timeout ()
                      (log-debug "Swank reader: read timeout, retrying"))
                    (cl-tron-mcp/swank-protocol:swank-read-error (e)
                      (let ((inner
                              (cl-tron-mcp/swank-protocol:swank-read-error-condition
                               e)))
                        (cond
                          #+sbcl
                          ((typep inner 'sb-sys:io-timeout)
                           (log-debug
                            "Swank reader: stream I/O timeout, retrying"))
                          ((or (not *swank-running*)
                               (/= generation *connection-generation*))
                           (log-debug
                            (format nil "Swank reader exiting: ~a" e))
                           (return))
                          (t
                           (log-error (format nil "Swank reader error: ~a" e))
                           (setf connection-failed-p t)
                           (return)))))
                    (end-of-file ()
                      (when (= generation *connection-generation*)
                        (setf connection-failed-p t)
                        (log-info "Swank connection closed (EOF)"))
                      (return))
                    (error (e)
                      (if (and *swank-running*
                               (= generation *connection-generation*))
                          (progn
                            (setf connection-failed-p t)
                            (log-error (format nil "Swank reader error: ~a" e)))
                          (log-debug
                           (format nil "Swank reader exiting: ~a" e)))
                      (return))))
      (log-debug "Swank reader thread exiting")
      (when connection-failed-p
        (multiple-value-bind
              (invalidated-p reconnect-generation host port)
            (invalidate-swank-connection
             :expected-io io
             :expected-socket socket
             :expected-generation generation)
          (when (and invalidated-p *reconnect-enabled*)
            (apply #'start-reconnect-worker
                   (append
                    (list :expected-generation reconnect-generation)
                    (when host (list :host host))
                    (when port (list :port port))))))))))

;;; ============================================================
;;; Heartbeat / Keepalive
;;; ============================================================

(defun heartbeat-loop (&optional (generation *connection-generation*))
  "Background thread: monitors connection activity without interrupting work.
Read EOF and failed writes are authoritative connection-failure signals.  Idle
time alone is not: a valid evaluation may run longer than several intervals."
  (log-debug "Swank heartbeat thread started")
  (loop while (and *heartbeat-running*
                   (= generation *connection-generation*))
        do (sleep *heartbeat-interval*)
           (when (and (= generation *connection-generation*)
                      (swank-connected-p))
             (let ((last-activity
                     (bordeaux-threads:with-lock-held (*connection-lock*)
                       *last-activity-time*)))
               (when (and last-activity
                          (> (- (get-unix-time) last-activity)
                             (* *heartbeat-interval* 2)))
                 (log-debug "Swank connection is idle")))))
  (log-debug "Swank heartbeat thread exiting"))

;;; ============================================================
;;; Incoming Message Dispatch
;;; ============================================================

(defun dispatch-incoming-message (message &optional expected-generation)
  "Route an incoming Swank message to the appropriate handler."
  (when message
    (destructuring-bind (tag &rest args) message
      (case tag
        (:return
          (destructuring-bind (result id) args
            (fulfill-request id (list :result result) expected-generation)))
        (:debug
         (destructuring-bind (thread level condition restarts frames
                              &optional continuation-ids)
             args
           ;; enqueue-debugger-event is defined in swank-events.lisp (loaded after)
           (enqueue-debugger-event condition restarts frames
                                   :thread thread
                                   :level level
                                   :expected-generation expected-generation)
           (bordeaux-threads:with-lock-held (*request-lock*)
             ;; Swank supplies the originating evaluation's continuation IDs.
             ;; Using the newest local request instead lets a late debugger
             ;; event from a timed-out evaluation complete an unrelated call.
             (let* ((request-id
                      (and (or (null expected-generation)
                               (= expected-generation
                                  *connection-generation*))
                      (find-if (lambda (id)
                                 (gethash id *pending-requests*))
                               continuation-ids)))
                    (req (and request-id
                              (gethash request-id *pending-requests*))))
               (when req
                 (setf (swank-request-response req)
                       (list :result (list :debug t
                                           :thread thread
                                           :level level
                                           :condition condition
                                           :restarts restarts
                                           :frames frames))
                       (swank-request-completed-p req) t)
                 (bordeaux-threads:condition-notify
                  (swank-request-condition req)))))))
        (:write-string
         (destructuring-bind (string &optional target thread-id) args
           (declare (ignore thread-id))
           (handle-output string target expected-generation)))
        (:read-string
         ;; Swank is requesting input from the user.  We store the pending
         ;; request so that the caller can supply input via swank-provide-input.
         (destructuring-bind (thread-id tag) args
           (log-info (format nil "Swank requesting input (thread ~a tag ~a). Use swank_send_input to respond." thread-id tag))
           (bordeaux-threads:with-lock-held (*input-request-lock*)
             (when (or (null expected-generation)
                       (= expected-generation *connection-generation*))
               (push (cons thread-id tag) *pending-input-requests*)))))
        (:debug-activate
         (destructuring-bind (thread-id level selections) args
           (declare (ignore selections))
           (note-debugger-activation thread-id level expected-generation)))
        (:debug-return
         (destructuring-bind (thread-id level stepping-p) args
           (declare (ignore stepping-p))
           (note-debugger-return
            level
            :expected-generation expected-generation
            :expected-thread thread-id)))
        (:new-package
         (destructuring-bind (name prompt-string) args
           (declare (ignore prompt-string))
           (log-info (format nil "Swank package changed to ~a" name))))
        (:new-features
         ;; Sent after compile/load — ignore
         )
        (:indentation-update
         ;; Sent after compile — ignore
         )
        (:ping
         (destructuring-bind (thread-id tag) args
           (write-message `(:emacs-pong ,thread-id ,tag)
                          :expected-generation expected-generation)))
        (:invalid-rpc
         ;; Swank rejected a rex (e.g. thread-id not found).  Fulfil the
         ;; pending request with an error instead of leaving it to time out,
         ;; so the caller unblocks immediately.
         (destructuring-bind (id message-text) args
           (log-warn (format nil "Swank rejected request ~a: ~a" id message-text))
           (fulfill-request id
                            (list :error t
                                  :message
                                  (format nil "Invalid RPC: ~a" message-text))
                            expected-generation)))
        (t
         (log-warn (format nil "Unhandled Swank message: ~s" message)))))))

;;; ============================================================
;;; Request Sending
;;; ============================================================

(defun send-request (form &key (package "CL-USER") (thread t) (timeout *default-eval-timeout*))
  "Send :emacs-rex request and wait synchronously for the response.
FORM     — S-expression to evaluate.
PACKAGE  — package name string.
THREAD   — which thread to use (t, :repl-thread, or integer).
TIMEOUT  — maximum seconds to wait (default: *default-eval-timeout*)."
  (let* ((id (make-request-id))
         (req (make-swank-request :id id
                                  :condition (bordeaux-threads:make-condition-variable)
                                  :response nil
                                  :completed-p nil)))
    (multiple-value-bind (registered-p generation)
        (register-request req)
      (unless registered-p
        (return-from send-request
          (cl-tron-mcp/resources:make-error "SWANK_NOT_CONNECTED")))
      (unwind-protect
           (handler-case
               (progn
                 (write-message `(:emacs-rex ,form ,package ,thread ,id)
                                :expected-generation generation)
                 (wait-for-response id :timeout timeout))
             (bordeaux-threads:timeout (e)
               (error e))
             (error (e)
               (cl-tron-mcp/resources:make-error
                "INTERNAL_ERROR"
                :details (list :error (princ-to-string e)))))
        ;; Completed synchronous requests used to remain here forever.  Besides
        ;; leaking one entry per call, that left *CURRENT-REQUEST-ID* pointing at
        ;; a request which could receive an unrelated later debugger event.
        (bordeaux-threads:with-lock-held (*request-lock*)
          (remhash id *pending-requests*)
          (when (eql *current-request-id* id)
            (setf *current-request-id* nil)))))))

(defun handle-output (string target &optional expected-generation)
  "Handle :write-string output from Swank."
  (when (enqueue-output-event string target expected-generation)
    (when *output-callback*
      (handler-case
          (funcall *output-callback* string target)
        (error (e)
          (log-error (format nil "Output callback error: ~a" e)))))))

;;; ============================================================
;;; Asynchronous Evaluation
;;; ============================================================

(defun send-request-async (form &key (package "CL-USER") (thread t))
  "Send :emacs-rex request asynchronously; return request ID immediately.
Use get-async-result with the returned ID to retrieve the result later."
  (let* ((id (make-request-id))
         (req (make-swank-request :id id
                                  :condition (bordeaux-threads:make-condition-variable)
                                  :response nil
                                  :completed-p nil))
         (sent-p nil))
    (multiple-value-bind (registered-p generation)
        (register-request req)
      (unless registered-p
        (return-from send-request-async
          (cl-tron-mcp/resources:make-error "SWANK_NOT_CONNECTED")))
      (unwind-protect
           (handler-case
               (progn
                 (write-message `(:emacs-rex ,form ,package ,thread ,id)
                                :expected-generation generation)
                 (setf sent-p t)
                 id)
             (bordeaux-threads:timeout (e)
               (error e))
             (error (e)
               (cl-tron-mcp/resources:make-error
                "INTERNAL_ERROR"
                :details (list :error (princ-to-string e)))))
        (unless sent-p
          (bordeaux-threads:with-lock-held (*request-lock*)
            (remhash id *pending-requests*)
            (when (eql *current-request-id* id)
              (setf *current-request-id* nil))))))))

(defun get-async-result (id &key (timeout *default-eval-timeout*))
  "Get the result of an async request by ID, waiting up to TIMEOUT seconds."
  (let ((req (bordeaux-threads:with-lock-held (*request-lock*)
               (gethash id *pending-requests*))))
    (unless req
      (return-from get-async-result
        (cl-tron-mcp/resources:make-error "REQUEST_NOT_FOUND"
                                     :details (list :request-id id))))
    (unwind-protect
         (if (bordeaux-threads:with-lock-held (*request-lock*)
               (swank-request-completed-p req))
             (swank-request-response req)
             (wait-for-response id :timeout timeout))
      (bordeaux-threads:with-lock-held (*request-lock*)
        (remhash id *pending-requests*)
        (when (eql *current-request-id* id)
          (setf *current-request-id* nil))))))
