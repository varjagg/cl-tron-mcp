;;;; src/swank/swank-connection.lisp - Swank connection management
;;;;
;;;; This file handles the low-level connection to a running SBCL+Swank
;;;; session: socket setup, connection state variables, connect/disconnect,
;;;; and the raw protocol I/O functions (read-packet, write-message).
;;;;
;;;; SWANK package placeholder:
;;;;   The MCP process does NOT load Swank locally — it only serialises
;;;;   symbols like SWANK:EVAL-AND-GRAB-OUTPUT that the remote Swank server
;;;;   will resolve.  We create a minimal placeholder :swank package for this.
;;;;
;;;; Load order within src/swank/:
;;;;   swank-connection  ← this file
;;;;   swank-rpc         (request-response correlation, reader loop)
;;;;   swank-events      (event queue, reconnect)
;;;;   swank-api         (high-level RPC operations, MCP wrappers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :bordeaux-threads :silent t))

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; SWANK package placeholder
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :swank)
    (make-package :swank :use '())))

;;; ============================================================
;;; I/O package for reading Swank responses
;;; ============================================================
;;;
;;; Only imports NIL, T and QUOTE so arbitrary symbols in responses
;;; don't intern into well-known packages.

(defvar *swank-io-package* (find-package :swank-io-package))
(unless *swank-io-package*
  (setf *swank-io-package*
        (make-package :swank-io-package :use '())))

(defun swank-sym (name)
  "Create a symbol in the SWANK placeholder package for protocol messages.
Example: (swank-sym \"EVAL-AND-GRAB-OUTPUT\") => SWANK:EVAL-AND-GRAB-OUTPUT"
  (declare (type string name))
  (intern (string-upcase name) :swank))

;;; ============================================================
;;; Utility
;;; ============================================================

(defun get-unix-time ()
  "Return current Unix timestamp as an integer."
  #+sbcl  (sb-ext:get-time-of-day)
  #+ccl   (ccl:get-time-of-day)
  #+ecl   (- (get-universal-time) 2208988800)
  #-(or sbcl ccl ecl) (- (get-universal-time) 2208988800))

(defun join-thread-with-timeout (thread timeout)
  "Join THREAD for at most TIMEOUT seconds; return true when it stopped."
  (handler-case
      (progn
        (bordeaux-threads:with-timeout (timeout)
          (bordeaux-threads:join-thread thread))
        t)
    (bordeaux-threads:timeout () nil)))

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *swank-socket* nil
  "Socket connection to Swank server.")

(defvar *swank-io* nil
  "Bidirectional I/O stream for Swank communication.")

(defvar *swank-host* nil
  "Host of the installed Swank connection.")

(defvar *swank-port* nil
  "Port of the installed Swank connection.")

(defvar *swank-connected* nil
  "Connection flag — T when connected to a Swank server.")

(defvar *swank-reader-thread* nil
  "Background thread that reads incoming Swank messages.")

(defvar *swank-running* nil
  "Control flag for reader thread — set to NIL to stop.")

(defvar *connection-lock* (bordeaux-threads:make-lock "swank-connection")
  "Lock for synchronising access to connection state variables.")
(defvar *connection-generation* 0
  "Token distinguishing background threads from successive connections.")

;;; Request tracking (for synchronous RPC calls)
(defvar *pending-requests* (make-hash-table :test 'eql)
  "Hash table mapping request IDs to swank-request structures.")
(defvar *request-lock* (bordeaux-threads:make-lock "swank-requests")
  "Lock for synchronising access to *pending-requests*.")
(defvar *write-lock* (bordeaux-threads:make-lock "swank-writes")
  "Lock covering each complete length-prefixed Swank packet write.")
(defvar *current-request-id* nil
  "ID of the most recent request sent (used to match :debug events to requests).")
(defvar *next-request-id* 1
  "Counter for generating unique request IDs.")

;;; Event queue for asynchronous messages (:debug, :write-string, etc.)
(defvar *event-queue* (make-array 100 :adjustable t :fill-pointer 0)
  "Queue of async events received from Swank (:debug, :write-string, etc.).")
(defvar *event-mutex* (bordeaux-threads:make-lock "swank-events")
  "Lock for synchronising access to *event-queue*.")
(defvar *event-condition* (bordeaux-threads:make-condition-variable)
  "Condition variable for waiting on events.")
(defvar *event-processor-running* nil
  "Flag indicating if the event processor thread is running.")
(defvar *event-processor-thread* nil
  "Thread that processes async events.")
(defvar *max-event-queue-size* 1000
  "Maximum number of events to keep in the queue before dropping oldest.")

;;; Pending input requests (for :read-string Swank messages)
(defvar *pending-input-requests* nil
  "List of pending (thread-id . tag) pairs for :read-string requests.")
(defvar *input-request-lock* (bordeaux-threads:make-lock "swank-input")
  "Lock for synchronising access to *pending-input-requests*.")

;;; Debugger state
(defvar *debugger-thread* nil
  "Thread ID currently in the debugger, or NIL.")
(defvar *debugger-level* 0
  "Current debugger level (0 = not in debugger).")

(defvar *debugger-episode* 0
  "Monotonic token distinguishing successive debugger entries.")

;;; ============================================================
;;; Timeout and Heartbeat Configuration
;;; ============================================================

(defvar *default-eval-timeout* 30
  "Default timeout for evaluation requests in seconds.")

(defvar *heartbeat-interval* 60
  "Heartbeat interval in seconds for keepalive pings.")

(defvar *last-activity-time* nil
  "Timestamp of last activity from Swank server.")

(defvar *heartbeat-thread* nil
  "Thread that sends periodic heartbeat pings.")

(defvar *heartbeat-running* nil
  "Control flag for heartbeat thread.")

;;; ============================================================
;;; Reconnection Configuration
;;; ============================================================

(defvar *reconnect-enabled* t
  "Whether automatic reconnection is enabled.")

(defvar *reconnect-max-attempts* 5
  "Maximum number of reconnection attempts.")

(defvar *reconnect-delay* 5
  "Initial delay between reconnection attempts in seconds.")

(defvar *reconnect-attempt-count* 0
  "Current reconnection attempt counter.")

(defvar *reconnect-thread* nil
  "Background reconnect worker started after an outbound write failure.")

(defvar *reconnect-generation* nil
  "Connection generation owned by *RECONNECT-THREAD*.")

;;; ============================================================
;;; Output Streaming
;;; ============================================================

(defvar *output-callback* nil
  "Callback (string target) called when output is received from Swank.")

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun clear-swank-session-state ()
  "Clear state whose identifiers belong to the detached Swank connection."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (setf (fill-pointer *event-queue*) 0
          *debugger-thread* nil
          *debugger-level* 0)
    (bordeaux-threads:condition-notify *event-condition*))
  (bordeaux-threads:with-lock-held (*input-request-lock*)
    (setf *pending-input-requests* nil))
  ;; Defined in swank-rpc.lisp, after the SWANK-REQUEST structure.  Calls can
  ;; only occur after the complete system has loaded.
  (fail-pending-requests "Swank connection closed"))

(defun swank-connect (&key (host "127.0.0.1")
                           (port (cl-tron-mcp/config:get-config :swank-port))
                           (timeout 10)
                           expected-generation)
  "Connect to a running SBCL with Swank loaded.
On the SBCL side: (ql:quickload :swank) (swank:create-server :port 4005)

Returns: Connection status plist or error."
  (let (socket io reader-thread event-thread heartbeat-thread
        starting-generation retry-generation result)
    ;; Do not hold the state lock across a potentially slow TCP connect.  The
    ;; generation captured here makes installation conditional on no manual
    ;; disconnect or competing connect having superseded this attempt.
    (bordeaux-threads:with-lock-held (*connection-lock*)
      (when (and expected-generation
                 (/= expected-generation *connection-generation*))
        (return-from swank-connect
          (list :cancelled t :message "Reconnection was superseded")))
      (when (and *swank-connected* *swank-socket*)
        (return-from swank-connect
          (cl-tron-mcp/resources:make-error "SWANK_ALREADY_CONNECTED")))
      (setf starting-generation *connection-generation*))
    (handler-case
        (progn
          (setf socket
                (usocket:socket-connect
                 host port :timeout timeout :element-type '(unsigned-byte 8))
                io (usocket:socket-stream socket))
          (bordeaux-threads:with-lock-held (*connection-lock*)
            (cond
              ((/= starting-generation *connection-generation*)
               (setf result
                     (list :cancelled t
                           :message "Connection attempt was superseded")))
              ((and *swank-connected* *swank-socket*)
               (setf result
                     (cl-tron-mcp/resources:make-error
                      "SWANK_ALREADY_CONNECTED")))
              (t
               (let ((generation (incf *connection-generation*)))
                 (handler-case
                     (progn
                       (setf *swank-socket* socket
                             *swank-io* io
                             *swank-host* host
                             *swank-port* port
                             *swank-connected* t
                             *swank-running* t
                             *last-activity-time* (get-unix-time))
                       ;; These functions are defined in later Swank source
                       ;; files and are resolved when a connection is opened.
                       (setf reader-thread
                             (cl-tron-mcp/logging:make-diagnostic-thread
                              (lambda ()
                                (swank-reader-loop generation io socket))
                              :name "swank-reader")
                             *swank-reader-thread* reader-thread
                             *event-processor-running* t
                             event-thread
                             (cl-tron-mcp/logging:make-diagnostic-thread
                              (lambda () (swank-event-processor generation))
                              :name "swank-event-processor")
                             *event-processor-thread* event-thread
                             *heartbeat-running* t
                             heartbeat-thread
                             (cl-tron-mcp/logging:make-diagnostic-thread
                             (lambda () (heartbeat-loop generation))
                              :name "swank-heartbeat")
                             *heartbeat-thread* heartbeat-thread
                             *reconnect-attempt-count* 0
                             result
                             (list :success t
                                   :host host
                                   :port port
                                   :generation generation
                                   :message
                                   (format nil
                                           "Connected to Swank at ~a:~a"
                                           host port))))
                   (error (e)
                     ;; Cancel any workers already started, then detach every
                     ;; published resource as one failed installation.
                     (setf *swank-running* nil
                           *event-processor-running* nil
                           *heartbeat-running* nil
                           *swank-connected* nil
                           *swank-socket* nil
                           *swank-io* nil
                           *swank-host* nil
                           *swank-port* nil
                           *swank-reader-thread* nil
                           *event-processor-thread* nil
                           *heartbeat-thread* nil
                           *last-activity-time* nil)
                     (setf retry-generation
                           (incf *connection-generation*))
                     (clear-swank-session-state)
                     (setf result
                           (cl-tron-mcp/resources:make-error
                            "SWANK_CONNECTION_FAILED"
                            :details
                            (list :error (princ-to-string e)))))))))))
      (error (e)
        (setf result
              (cl-tron-mcp/resources:make-error
               "SWANK_CONNECTION_FAILED"
               :details (list :error (princ-to-string e))))))
    (unless (getf result :success)
      (when io
        (ignore-errors (close io :abort t)))
      (when socket
        (ignore-errors (usocket:socket-close socket)))
      (dolist (thread (list reader-thread event-thread heartbeat-thread))
        (when (and thread
                   (not (eq thread (bordeaux-threads:current-thread))))
          (ignore-errors
            (join-thread-with-timeout thread 2)))))
    (when (getf result :success)
      (log-info (format nil "Connected to Swank at ~a:~a" host port)))
    (values result retry-generation)))

(defun swank-disconnect ()
  "Disconnect from the Swank server and stop all background threads."
  (let (io socket reader-thread event-thread heartbeat-thread reconnect-thread)
    ;; Detach all resources as one state transition.  Cleanup below uses only
    ;; these snapshots, so it cannot close or clear a connection installed later.
    (bordeaux-threads:with-lock-held (*connection-lock*)
      (setf io *swank-io*
            socket *swank-socket*
            reader-thread *swank-reader-thread*
            event-thread *event-processor-thread*
            heartbeat-thread *heartbeat-thread*
            reconnect-thread *reconnect-thread*
            *swank-running* nil
            *event-processor-running* nil
            *heartbeat-running* nil
            *swank-connected* nil
            *swank-socket* nil
            *swank-io* nil
            *swank-host* nil
            *swank-port* nil
            *swank-reader-thread* nil
            *event-processor-thread* nil
            *heartbeat-thread* nil
            *last-activity-time* nil)
      (incf *connection-generation*)
      (clear-swank-session-state))
    (when io
      (ignore-errors (close io :abort t)))
    (flet ((join-old-thread (thread)
             (when (and thread
                        (not (eq thread (bordeaux-threads:current-thread))))
               (ignore-errors
                 (join-thread-with-timeout thread 2)))))
      (join-old-thread reader-thread)
      (join-old-thread event-thread)
      (join-old-thread heartbeat-thread)
      (join-old-thread reconnect-thread))
    (when socket
      (ignore-errors (usocket:socket-close socket))))
  (log-info "Disconnected from Swank server")
  (list :success t :message "Disconnected from Swank server"))

(defun swank-connected-p ()
  "Return T if connected to Swank."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (and *swank-connected* *swank-socket* *swank-io*)))

(defun live-connection-generation ()
  "Return the installed connection generation and true, or NIL values."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (when (and *swank-connected* *swank-running*
               *swank-socket* *swank-io*)
      (values *connection-generation* t))))

(defun swank-status ()
  "Return current Swank connection status as a plist."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (list :connected (and *swank-connected* *swank-socket* *swank-io*)
          :has-connection (not (null *swank-socket*))
          :host *swank-host*
          :port *swank-port*
          :reader-thread-alive (and *swank-reader-thread*
                                    (bordeaux-threads:thread-alive-p *swank-reader-thread*))
          :event-processor-alive (and *event-processor-thread*
                                      (bordeaux-threads:thread-alive-p *event-processor-thread*))
          :heartbeat-alive (and *heartbeat-thread*
                                (bordeaux-threads:thread-alive-p *heartbeat-thread*))
          :last-activity *last-activity-time*)))

;;; ============================================================
;;; Message Protocol I/O
;;; ============================================================

(defun read-packet ()
  "Read and parse a single Swank packet from *swank-io*."
  (cl-tron-mcp/swank-protocol:read-message *swank-io* *swank-io-package*))

(defun invalidate-swank-connection
    (&key
       (expected-io nil expected-io-p)
       (expected-socket nil expected-socket-p)
       (expected-generation nil expected-generation-p))
  "Close the current connection when it matches the expected connection.

Expected values keep cleanup from a stale writer from closing a replacement
connection installed after that writer began."
  (let (io socket host port invalidated-p invalidated-generation)
    (bordeaux-threads:with-lock-held (*connection-lock*)
      (when (and (or (not expected-io-p)
                     (eq *swank-io* expected-io))
                 (or (not expected-socket-p)
                     (eq *swank-socket* expected-socket))
                 (or (not expected-generation-p)
                     (= *connection-generation* expected-generation)))
        (setf io *swank-io*
              socket *swank-socket*
              host *swank-host*
              port *swank-port*
              *swank-running* nil
              *event-processor-running* nil
              *heartbeat-running* nil
              *swank-connected* nil
              *swank-io* nil
              *swank-socket* nil
              *swank-host* nil
              *swank-port* nil
              invalidated-p t)
        (setf invalidated-generation (incf *connection-generation*))
        ;; Keep detachment and session-state reset atomic with respect to a
        ;; replacement connection.  Otherwise a reconnect could install its
        ;; state before this cleanup runs.
        (clear-swank-session-state)))
    (when invalidated-p
      (when io
        (ignore-errors (close io :abort t)))
      (when socket
        (ignore-errors (usocket:socket-close socket))))
    (values invalidated-p invalidated-generation host port)))

(defun write-message (message &key expected-generation)
  "Write MESSAGE to Swank using proper length-prefixed encoding.
When EXPECTED-GENERATION is supplied, silently discard a message produced by
an obsolete reader instead of sending it to a replacement connection."
  (let (reconnect-generation reconnect-host reconnect-port)
    ;; Schedule recovery only after *WRITE-LOCK* has unwound.  A reconnect may
    ;; itself write during initialization and must never wait behind this frame.
    (unwind-protect
         (bordeaux-threads:with-lock-held (*write-lock*)
           (multiple-value-bind (io socket generation)
               (bordeaux-threads:with-lock-held (*connection-lock*)
                 (unless (and *swank-connected* *swank-running*
                              *swank-io* *swank-socket*)
                   (error "Swank is not connected"))
                 (when (and expected-generation
                            (/= expected-generation *connection-generation*))
                   (return-from write-message nil))
                 (values *swank-io* *swank-socket*
                         *connection-generation*))
             (let ((completed-p nil))
               (unwind-protect
                    (progn
                      (cl-tron-mcp/swank-protocol:write-message
                       message
                       (or *swank-io-package* (find-package :cl))
                       io)
                      (setf completed-p t)
                      t)
                 ;; No queued writer may append after an interrupted frame, so
                 ;; detach while the complete packet-write lock is still held.
                 (unless completed-p
                   (multiple-value-bind
                         (invalidated-p generation host port)
                       (invalidate-swank-connection
                        :expected-io io
                        :expected-socket socket
                        :expected-generation generation)
                     (when invalidated-p
                       (setf reconnect-generation generation
                             reconnect-host host
                             reconnect-port port))))))))
      (when (and reconnect-generation reconnect-host reconnect-port
                 *reconnect-enabled*)
        ;; Recovery failure must not replace the packet-write condition which
        ;; caused this unwind; the caller still needs the original I/O error.
        (handler-case
            (start-reconnect-worker
             :expected-generation reconnect-generation
             :host reconnect-host
             :port reconnect-port)
          (error (e)
            (ignore-errors
              (log-error
               (format nil "Could not start Swank reconnect worker: ~a"
                       e)))))))))
