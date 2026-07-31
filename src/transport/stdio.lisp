;;;; src/transport/stdio.lisp
;;;; Stdio transport for MCP. CRITICAL: stdout must contain only newline-delimited
;;;; JSON-RPC messages; all other output (logs, errors) goes via log4cl (configured
;;;; to stderr for stdio by ensure-log-to-stream in server.lisp).

(in-package :cl-tron-mcp/transport)

(defvar *transport* nil)
(defvar *transport-type* :stdio)
(defvar *running* nil)
(defvar *stdio-state-lock*
  (bordeaux-threads:make-lock "mcp-stdio-state"))
(defvar *stdio-output-lock*
  (bordeaux-threads:make-lock "mcp-stdio-output"))
(defvar *stdio-generation* 0
  "Monotonic token identifying the current or pending stdio session.")
(defvar *stdio-protocol-output* nil
  "The stdout stream reserved for newline-delimited JSON-RPC responses.")
(defvar *stdio-protocol-input* nil
  "The caller-owned stdin stream read by the active JSON-RPC loop.")
(defvar *stdio-thread* nil
  "Thread currently owning the stdio protocol read loop.")

(defun reserve-stdio-session ()
  "Reserve and return a token for a new stdio session."
  (bordeaux-threads:with-lock-held (*stdio-state-lock*)
    (setf *running* nil)
    (incf *stdio-generation*)))

(defun dispatch-stdio-line (line handler output)
  "Decode and dispatch one newline-delimited JSON-RPC message."
  (handler-case
      (let* ((message (cl-tron-mcp/protocol:parse-message line))
             (response (funcall handler message)))
        (when response
          (funcall output response)))
    (com.inuoe.jzon:json-error (e)
      (cl-tron-mcp/logging:log-error
       (format nil "JSON parse error: ~a" e))
      (funcall output
               (cl-tron-mcp/protocol:make-error-response
                :null -32700 "Parse error")))
    (error (e)
      (cl-tron-mcp/logging:log-error
       (format nil "Error: ~a" e)))))

(defun start-stdio-transport
    (&key (handler #'cl-tron-mcp/protocol:handle-message)
          (output #'cl-tron-mcp/transport::send-message-via-stdio)
          session-token)
  "Start stdio transport. Activity is logged via log4cl (stderr); only JSON responses go to stdout."
  ;; Keep this invariant local to the transport as well as START-SERVER.  The
  ;; transport is public and is also started directly in tests and integrations.
  (let* ((owner (bordeaux-threads:current-thread))
         (protocol-input *standard-input*)
         (token (or session-token (reserve-stdio-session)))
         (registered-p
           (bordeaux-threads:with-lock-held (*stdio-state-lock*)
             (when (= token *stdio-generation*)
               (setf *running* t
                     *stdio-thread* owner
                     *stdio-protocol-input* protocol-input)
               t))))
    ;; STOP may invalidate a combined worker after its thread is published but
    ;; before it enters this function.
    (unless registered-p
      (return-from start-stdio-transport nil))
    (cl-tron-mcp/logging:ensure-log-to-stream *error-output*)
    (cl-tron-mcp/logging:log-info
     "[MCP] Starting stdio transport (MCP protocol)")
    (unwind-protect
         (let* ((*stdio-protocol-output* *standard-output*)
                  ;; Diagnostic readers see EOF.  MCP stdin belongs exclusively
                  ;; to the JSON-RPC loop and must never be consumed by a prompt.
                  (diagnostic-input (make-string-input-stream ""))
                  (diagnostic-io
                    (make-two-way-stream diagnostic-input *error-output*))
                  ;; Loaded libraries commonly write to these streams.  During
                  ;; stdio dispatch they are diagnostics, never protocol output.
                  (*standard-input* diagnostic-input)
                  (*standard-output* *error-output*)
                  (*trace-output* *error-output*)
                  (*terminal-io* diagnostic-io)
                  (*debug-io* diagnostic-io)
                  (*query-io* diagnostic-io)
                  (line-buffer
                    (make-array 256 :element-type 'character
                                    :adjustable t :fill-pointer 0)))
           (labels ((emit-response (response)
                      ;; A handler may finish after this transport was stopped
                      ;; and replaced.  Its response belongs to the old owner.
                      ;; Serialize complete response writes, but never hold the
                      ;; state lock across client I/O: FORCE-OUTPUT may block
                      ;; under pipe backpressure and STOP must remain prompt.
                      (bordeaux-threads:with-lock-held (*stdio-output-lock*)
                        (let ((active-p
                                (bordeaux-threads:with-lock-held
                                    (*stdio-state-lock*)
                                  (and *running*
                                       (= token *stdio-generation*)
                                       (eq *stdio-thread* owner)))))
                          (when active-p
                            (funcall output response)))))
                    (dispatch-buffer ()
                      ;; Accept a final message without a newline and normalize
                      ;; CRLF without altering carriage returns inside JSON.
                      (when (and (plusp (fill-pointer line-buffer))
                                 (char= (aref line-buffer
                                              (1- (fill-pointer line-buffer)))
                                        #\Return))
                        (decf (fill-pointer line-buffer)))
                      (dispatch-stdio-line
                       (coerce line-buffer 'string) handler #'emit-response)
                      (setf (fill-pointer line-buffer) 0)))
             ;; READ-CHAR-NO-HANG keeps shutdown non-destructive: STOP only
             ;; flips *RUNNING* and never closes the caller-owned process stdin.
             (loop
               (multiple-value-bind (character active-p)
                   ;; Hold the state lock across the nonblocking read so a
                   ;; replacement cannot claim the session between ownership
                   ;; validation and character consumption.
                   (bordeaux-threads:with-lock-held (*stdio-state-lock*)
                     (if (and *running*
                              (= token *stdio-generation*)
                              (eq *stdio-thread* owner))
                         (values
                          (read-char-no-hang protocol-input nil :eof)
                          t)
                         (values nil nil)))
                 (unless active-p
                   (return))
                 (let ((character character))
                        (cond
                          ((eq character :eof)
                           (when (plusp (fill-pointer line-buffer))
                             (dispatch-buffer))
                           (return))
                          ((null character)
                           (sleep 0.01))
                          ((char= character #\Newline)
                           (dispatch-buffer))
                          (t
                           (vector-push-extend character line-buffer))))))))
      (bordeaux-threads:with-lock-held (*stdio-state-lock*)
        (when (and (= token *stdio-generation*)
                   (eq *stdio-thread* owner))
          (setf *running* nil
                *stdio-thread* nil
                *stdio-protocol-input* nil))))))

(defun stop-stdio-transport ()
  "Stop stdio transport."
  (bordeaux-threads:with-lock-held (*stdio-state-lock*)
    (incf *stdio-generation*)
    (setf *running* nil
          *stdio-thread* nil
          *stdio-protocol-input* nil)))

(defun send-message-via-stdio (message)
  "Send a single JSON-RPC message to stdout. CRITICAL: This is the only place that must write to stdout for stdio transport.
   Handlers return already-serialized JSON strings; write as-is to avoid double-encoding (Cursor expects object, not string)."
  (let ((stream (or *stdio-protocol-output* *standard-output*)))
    (format stream "~a~%"
            (if (stringp message) message (cl-tron-mcp/json-compat:to-json message)))
    (force-output stream)))
