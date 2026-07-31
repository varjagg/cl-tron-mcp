;;;; src/logging/core.lisp

(in-package :cl-tron-mcp/logging)

(defvar *log-config* (list :level :info :package nil :appender :console))

(defvar *root-log-level* :info
  "The configured root level, independent of package-specific overrides.")

(defvar *log-output-stream* nil
  "Stream selected by ENSURE-LOG-TO-STREAM, or NIL for log4cl's default.

The stdio MCP transport stores its stderr stream here.  Subsequent calls to
LOG-CONFIGURE must retain that stream: stdout is the JSON-RPC transport and a
single log line there corrupts the protocol.")

(defun configure-root-logger (level)
  "Configure log4cl's root logger at LEVEL without losing its output stream."
  (log4cl:clear-logging-configuration)
  (if *log-output-stream*
      (log:config :stream *log-output-stream* level)
      (log:config level)))

(defun package-logger (package)
  "Return log4cl's logger for PACKAGE, creating it when necessary."
  (let ((pkg (find-package (string-upcase package))))
    (unless pkg
      (error "Package ~a does not exist" package))
    ;; LOG:CONFIG accepts logger objects, not package objects.  Log4cl does not
    ;; export its dynamic package-logger lookup, so use the same constructor its
    ;; configurator uses internally.
    (log4cl::instantiate-logger
     pkg (log4cl::make-package-categories pkg) t t)))

(defun log-package-message (level message package)
  "Emit MESSAGE at LEVEL through PACKAGE's runtime logger."
  (let* ((pkg (find-package (string-upcase package)))
         (logger (package-logger package))
         (numeric-level (log4cl:make-log-level level)))
    (when (log4cl::is-enabled-for logger numeric-level)
      (flet ((emit (stream)
               (princ message stream)))
        (declare (dynamic-extent #'emit))
        (log4cl::log-with-logger logger numeric-level #'emit pkg)))))

(defun call-with-diagnostic-io (function error-output)
  "Call FUNCTION with diagnostic output isolated from MCP stdin and stdout."
  (let* ((diagnostic-input (make-string-input-stream ""))
         (diagnostic-io
           (make-two-way-stream diagnostic-input error-output))
         (*standard-input* diagnostic-input)
         (*standard-output* error-output)
         (*error-output* error-output)
         (*trace-output* error-output)
         (*terminal-io* diagnostic-io)
         (*debug-io* diagnostic-io)
         (*query-io* diagnostic-io))
    (funcall function)))

(defun make-diagnostic-thread (function &key name)
  "Start FUNCTION in a thread whose non-protocol output goes to stderr."
  (let ((error-output *error-output*))
    (bordeaux-threads:make-thread
     (lambda ()
       (call-with-diagnostic-io function error-output))
     :name name)))

(defun log-configure (&key level package appender)
  "Configure logging for a package or globally.
   LEVEL: :trace, :debug, :info, :warn, :error, :fatal
   PACKAGE: package name string (nil for global)
   APPENDER: :console (default)"
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (when level
          (setq *log-config* (list :level level :package package :appender appender))
          (if package
              (log:config (package-logger package) level)
              (progn
                (setf *root-log-level* level)
                (configure-root-logger level))))
        (list :success t
              :config *log-config*))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun log-level ()
  "Get the root log level."
  *root-log-level*)

(defun log-info (message &key package)
  "Log info message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (log-package-message :info message package)
            (log:info message))
        (list :logged t :message message :level :info))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-debug (message &key package)
  "Log debug message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (log-package-message :debug message package)
            (log:debug message))
        (list :logged t :message message :level :debug))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-warn (message &key package)
  "Log warning message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (log-package-message :warn message package)
            (log:warn message))
        (list :logged t :message message :level :warn))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-error (message &key package)
  "Log error message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (log-package-message :error message package)
            (log:error message))
        (list :logged t :message message :level :error))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun get-log-config ()
  "Get current logging configuration."
  *log-config*)

(defun ensure-log-to-stream (stream)
  "Configure log4cl so root logger output goes to STREAM (e.g. *error-output*).
   CRITICAL for MCP stdio: call this before any logging when using stdio transport
   so that stdout contains only newline-delimited JSON-RPC messages and all server
   activity (startup, notifications, errors) goes to stderr via log4cl."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (setf *log-output-stream* stream)
        (configure-root-logger (log-level))
        (values))
    (error (e)
      (warn "Could not configure log4cl to stream: ~a" e))))
