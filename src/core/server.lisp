;;;; src/core/server.lisp
;;;; MCP server entry. CRITICAL for stdio: all [MCP] activity is logged via log4cl;
;;;; for :stdio we call ensure-log-to-stream(*error-output*) so log4cl writes to
;;;; stderr and stdout stays clean for JSON-RPC only.

(in-package :cl-tron-mcp/core)

(defvar *server-state* :stopped)
(defvar *current-transport* nil)
(defvar *transport-thread* nil)
(defvar *transport-thread-lock*
  (bordeaux-threads:make-lock "mcp-transport-thread"))

(defun join-thread-with-timeout (thread timeout)
  "Join THREAD for at most TIMEOUT seconds; return true when it stopped."
  (handler-case
      (progn
        (bordeaux-threads:with-timeout (timeout)
          (bordeaux-threads:join-thread thread))
        t)
    (bordeaux-threads:timeout () nil)))

(defun make-stdio-transport-thread ()
  "Start combined-mode stdio with explicit ownership of the process streams."
  (let ((protocol-input *standard-input*)
        (protocol-output *standard-output*)
        (diagnostic-output *error-output*)
        (start-lock (bordeaux-threads:make-lock "mcp-stdio-start"))
        (start-condition (bordeaux-threads:make-condition-variable))
        (session-token
          (cl-tron-mcp/transport::reserve-stdio-session))
        (published-p nil)
        thread)
    (setf thread
          (bordeaux-threads:make-thread
           (lambda ()
             ;; Do not let a fast EOF finish before the creator publishes the
             ;; handle that this thread is responsible for clearing.
             (bordeaux-threads:with-lock-held (start-lock)
               (loop until published-p
                     do (bordeaux-threads:condition-wait
                         start-condition start-lock)))
             ;; New Lisp threads do not portably inherit the creator's dynamic
             ;; stream bindings.  Re-establish them before stdio captures them.
             (let ((*standard-input* protocol-input)
                   (*standard-output* protocol-output)
                   (*error-output* diagnostic-output))
               (unwind-protect
                    (cl-tron-mcp/transport:start-stdio-transport
                     :session-token session-token)
                 (bordeaux-threads:with-lock-held
                     (*transport-thread-lock*)
                   (when (eq *transport-thread*
                             (bordeaux-threads:current-thread))
                     (setf *transport-thread* nil))))))
           :name "mcp-stdio-combined"))
    (bordeaux-threads:with-lock-held (*transport-thread-lock*)
      (setf *transport-thread* thread))
    (bordeaux-threads:with-lock-held (start-lock)
      (setf published-p t)
      (bordeaux-threads:condition-notify start-condition))
    thread))

(defun %http-startup-log (msg)
  (flet ((try (path)
           (ignore-errors
            (ensure-directories-exist (make-pathname :name nil :type nil :defaults path))
            (with-open-file (f path :direction :output 
                                    :if-exists :append 
                                    :if-does-not-exist :create)
              (write-line (format nil "~a ~a" (get-universal-time) msg) f)))))
    (or (try (merge-pathnames "reports/http-startup.log" (or (ignore-errors (truename #p"./")) *default-pathname-defaults*)))
        (try #p"/tmp/cl-tron-mcp-http-startup.log"))))

(defun start-server (&key (transport :combined) (port (cl-tron-mcp/config:get-config :http-port)))
  "Start the MCP server with the specified transport.
   TRANSPORT can be :combined (default), :stdio-only, :http-only, or :websocket.
   :combined runs stdio and HTTP simultaneously, but the HTTP transport keeps
   the process alive so shell launches do not depend on stdio input remaining open.
   PORT is used for HTTP/WebSocket transports.
   For stdio, log4cl is configured to stderr so stdout contains only JSON-RPC."
  (%http-startup-log (format nil "start-server entered transport=~a port=~a" transport port))
  (cl-tron-mcp/config:set-config :transport transport)
  (when (eq *server-state* :running)
    (cl-tron-mcp/logging:log-warn "[MCP] Server is already running")
    (return-from start-server))
  (setq *server-state* :running)
  (handler-case
      (progn
        (cl-tron-mcp/logging:ensure-log-to-stream *error-output*)
        (cl-tron-mcp/logging:log-info (format nil "[MCP] Starting server with ~a transport" transport))
        (case transport
          (:combined
           (setq *current-transport* :combined)
           
           (%http-startup-log "start-server: starting stdio in background")
           (make-stdio-transport-thread)

           (%http-startup-log "start-server: starting HTTP (blocking)")
           (cl-tron-mcp/transport:start-http-transport :port port :block t)
           (%http-startup-log "start-server: combined transport returned"))
          (:stdio-only
           (start-stdio-transport))
          (:stdio
           (start-stdio-transport))
          (:http-only
           (setq *current-transport* :http)
           (%http-startup-log "start-server: calling start-http-transport")
           (cl-tron-mcp/transport:start-http-transport :port port :block t)
           (%http-startup-log "start-server: start-http-transport returned"))
          (:http
           (setq *current-transport* :http)
           (%http-startup-log "start-server: calling start-http-transport")
           (cl-tron-mcp/transport:start-http-transport :port port :block t)
           (%http-startup-log "start-server: start-http-transport returned"))
          (:websocket
           (start-websocket-transport port))
          (t
           (cl-tron-mcp/logging:log-error (format nil "[MCP] Unknown transport: ~a" transport))
           (setq *server-state* :stopped)))
        *server-state*)
    (serious-condition (e)
      ;; Combined startup may already have published the stdio worker when HTTP
      ;; startup fails.  Run the ordinary shutdown path before clearing state.
      (ignore-errors (stop-server))
      (setq *server-state* :stopped)
      (setq *current-transport* nil)
      (cl-tron-mcp/logging:log-error (format nil "[MCP] Server error: ~a" e))
      (format *error-output* "~&[MCP] Server error: ~a~%" e)
      (force-output *error-output*)
      *server-state*)))

(defun stop-server ()
  "Stop the MCP server."
  (when (eq *server-state* :stopped)
    (cl-tron-mcp/logging:log-warn "[MCP] Server is not running")
    (return-from stop-server))
  (cl-tron-mcp/logging:log-info "[MCP] Stopping server...")
  (let ((transport-thread
          (bordeaux-threads:with-lock-held (*transport-thread-lock*)
            *transport-thread*)))
    (case *current-transport*
      (:combined
       (cl-tron-mcp/transport:stop-http-transport)
       (cl-tron-mcp/transport:stop-stdio-transport))
      (:stdio (cl-tron-mcp/transport:stop-stdio-transport))
      (:http (cl-tron-mcp/transport:stop-http-transport))
      (:websocket (cl-tron-mcp/transport:stop-websocket-transport)))
    (when (and transport-thread
               (not (eq transport-thread
                        (bordeaux-threads:current-thread))))
      (ignore-errors
        (join-thread-with-timeout transport-thread 2)))
    (when (or (null transport-thread)
              (not (bordeaux-threads:thread-alive-p transport-thread)))
      (bordeaux-threads:with-lock-held (*transport-thread-lock*)
        (when (eq *transport-thread* transport-thread)
          (setf *transport-thread* nil)))))
  (setq *current-transport* nil)
  (setq *server-state* :stopped)
  (cl-tron-mcp/logging:log-info "[MCP] Server stopped"))

(defun start-stdio-transport ()
  "Start stdio transport (sets *current-transport* to :stdio)."
  (setq *current-transport* :stdio)
  (cl-tron-mcp/transport:start-stdio-transport))

(defun start-websocket-transport (port)
  "Start WebSocket transport."
  (setq *current-transport* :websocket)
  (cl-tron-mcp/transport:start-websocket-transport :port port))

(defun get-server-state ()
  "Get current server state."
  *server-state*)

(defun get-transport-type ()
  "Get current transport type."
  *current-transport*)
