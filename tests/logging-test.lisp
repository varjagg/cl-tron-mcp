;;;; tests/logging-test.lisp - Unit tests for logging tools

(defpackage :cl-tron-mcp.tests.logging.child
  (:use :cl))

(in-package :cl-tron-mcp.tests.logging.child)

(defun emit-debug-log (message)
  (log:debug message))

(in-package :cl-tron-mcp/tests)

(deftest log-configure-test
    (testing "log-configure accepts valid log levels"
             (let ((result (cl-tron-mcp/logging:log-configure :level :info)))
               (ok (listp result))))
  (testing "log-configure with package param succeeds"
           (let ((result (cl-tron-mcp/logging:log-configure :level :debug :package "cl-tron-mcp/core")))
             (ok (listp result)))))

(deftest log-configure-preserves-stdio-stream-test
  (testing "log-configure cannot redirect MCP server logs back to stdout"
    (let ((stdout (make-string-output-stream))
          (stderr (make-string-output-stream))
          (saved-error *error-output*)
          (marker "tron-stderr-routing-marker")
          (stdout-text nil)
          (stderr-text nil))
      (unwind-protect
           (let ((*standard-output* stdout)
                 (*error-output* stderr))
             (cl-tron-mcp/logging:ensure-log-to-stream stderr)
             (cl-tron-mcp/logging:log-configure :level :info)
             (cl-tron-mcp/logging:log-info marker)
             (force-output stderr)
             (setf stdout-text (get-output-stream-string stdout)
                   stderr-text (get-output-stream-string stderr)))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error))
      (ok (zerop (length stdout-text)))
      (ok (search marker stderr-text)))))

(deftest package-log-configuration-preserves-root-test
  (testing "a package override neither changes nor clears the root logger"
    (let ((capture (make-string-output-stream))
          (saved-error *error-output*))
      (unwind-protect
           (progn
             (cl-tron-mcp/logging:ensure-log-to-stream capture)
             (cl-tron-mcp/logging:log-configure :level :warn)
             (cl-tron-mcp/logging:log-configure
              :level :debug :package "CL-TRON-MCP/CORE")
             (ok (eq :warn (cl-tron-mcp/logging:log-level)))
             (cl-tron-mcp/logging:log-debug
              "package-level-marker" :package "CL-TRON-MCP/CORE")
             (cl-tron-mcp/logging:log-info "suppressed-root-marker")
             (let ((text (get-output-stream-string capture)))
               (ok (search "package-level-marker" text))
               (ok (not (search "suppressed-root-marker" text)))))
        (cl-tron-mcp/logging:log-configure :level :info)
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error)))))

(deftest dotted-package-configuration-matches-log-macro-test
  (testing "a dotted package override configures its ordinary log macro logger"
    (let ((capture (make-string-output-stream))
          (saved-error *error-output*)
          (marker "dotted-package-log-marker"))
      (unwind-protect
           (progn
             (cl-tron-mcp/logging:ensure-log-to-stream capture)
             (cl-tron-mcp/logging:log-configure :level :warn)
             (cl-tron-mcp/logging:log-configure
              :level :debug :package "CL-TRON-MCP.TESTS.LOGGING.CHILD")
             (cl-tron-mcp.tests.logging.child::emit-debug-log marker)
             (ok (search marker (get-output-stream-string capture))))
        (cl-tron-mcp/logging:log-configure :level :info)
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error)))))

(deftest logging-tool-passes-message-positionally-test
  (testing "log_info accepts normal dotted package names and invokes the core API"
    (let ((capture (make-string-output-stream))
          (saved-error *error-output*)
          (marker "logging-tool-marker"))
      (unwind-protect
           (progn
             (cl-tron-mcp/logging:ensure-log-to-stream capture)
             (cl-tron-mcp/logging:log-configure :level :info)
             (let ((result
                     (cl-tron-mcp/tools:call-tool
                      "log_info"
                      (list :|message| marker
                            :|package| "CL-TRON-MCP.TESTS.LOGGING.CHILD"))))
               (ok (getf result :logged))
               (ok (search marker (get-output-stream-string capture)))))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error)))))

(deftest diagnostic-io-cannot-read-caller-input-test
  (testing "diagnostic work sees EOF and leaves its caller's input untouched"
    (let* ((protocol-input (make-string-input-stream (format nil "second request~%")))
           (diagnostic-output (make-string-output-stream))
           (*standard-input* protocol-input)
           (observed
             (cl-tron-mcp/logging:call-with-diagnostic-io
              (lambda () (read-line *standard-input* nil))
              diagnostic-output)))
      (ok (null observed))
      (ok (string= "second request" (read-line protocol-input nil))))))

(deftest log-info-test
    (testing "log-info writes without error"
             (let ((result (cl-tron-mcp/logging:log-info "test info message")))
               (ok (listp result)))))

(deftest log-debug-test
    (testing "log-debug writes without error"
             (let ((result (cl-tron-mcp/logging:log-debug "test debug message")))
               (ok (listp result)))))

(deftest log-warn-test
    (testing "log-warn writes without error"
             (let ((result (cl-tron-mcp/logging:log-warn "test warn message")))
               (ok (listp result)))))

(deftest log-error-test
    (testing "log-error writes without error"
             (let ((result (cl-tron-mcp/logging:log-error "test error message")))
               (ok (listp result)))))

(deftest log-level-test
    (testing "log-level returns current level"
             (let ((result (cl-tron-mcp/logging:log-level)))
               ;; log-level returns a keyword or string
               (ok (not (null result))))))
