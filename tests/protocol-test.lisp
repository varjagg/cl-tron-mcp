;;;; tests/protocol-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest message-parsing-test
    (testing "JSON messages can be parsed")
  (ok t))

(deftest response-creation-test
    (testing "Response is created correctly"
             (let ((response (cl-tron-mcp/protocol:parse-message
                              (cl-tron-mcp/protocol:make-response 1 "result"))))
               (ok (equal (getf response :|id|) 1)))))

(deftest error-response-test
    (testing "Error response is created correctly"
             (let ((response (cl-tron-mcp/protocol:parse-message
                              (cl-tron-mcp/protocol:make-error-response 1 -32000 "error"))))
               (ok (equal (getf response :|id|) 1))
               (ok (getf response :|error|)))))

(deftest internal-request-error-preserves-id-test
  (testing "internal dispatch errors remain correlated with a parsed request"
    (let ((original-handler
            (symbol-function 'cl-tron-mcp/protocol:handle-request)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/protocol:handle-request)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "dispatch failed")))
             (let* ((response
                      (cl-tron-mcp/protocol:parse-message
                       (cl-tron-mcp/protocol:handle-message
                        (list :|jsonrpc| "2.0"
                              :|id| 37
                              :|method| "ping"))))
                    (error-object (getf response :|error|)))
               (ok (= 37 (getf response :|id|)))
               (ok (= -32603 (getf error-object :|code|)))))
        (setf (symbol-function 'cl-tron-mcp/protocol:handle-request)
              original-handler)))))

(deftest swank-read-timeout-remains-specific-test
  (testing "protocol timeouts reach the reader's nonfatal timeout handler"
    (let ((condition
            (handler-case
                (cl-tron-mcp/swank-protocol:read-packet
                 (make-string-input-stream "") :timeout -1)
              (condition (e) e))))
      (ok (typep condition
                 'cl-tron-mcp/swank-protocol:swank-read-timeout)))))

(deftest tool-execution-timeout-test
  (testing "tool execution is interrupted at its deadline"
    (let ((start (get-internal-real-time)))
      (ok (typep
           (handler-case
               (cl-tron-mcp/tools::call-with-timeout
                (lambda () (sleep 2)) 0.05)
             (cl-tron-mcp/tools:timeout-error (e) e))
           'cl-tron-mcp/tools:timeout-error))
      (ok (< (/ (- (get-internal-real-time) start)
                (float internal-time-units-per-second))
             0.5)))))

(deftest tool-timeout-preserves-request-bindings-test
  (testing "timed execution retains the request thread's output binding"
    (let ((capture (make-string-output-stream)))
      (let ((*standard-output* capture))
        (cl-tron-mcp/tools::call-with-timeout
         (lambda () (format t "bound output")) 1))
      (ok (string= "bound output" (get-output-stream-string capture))))))

(deftest tool-executor-enforces-timeout-test
  (testing "the JSON-RPC executor reports a deadline exceeded error"
    (let ((original-call-tool
            (symbol-function 'cl-tron-mcp/tools:call-tool))
          (cl-tron-mcp/tools:*default-tool-timeout* 0.05)
          (cl-tron-mcp/tools::*tool-timeout-cleanup-grace* 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/tools:call-tool)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     ;; The inner catch-all mirrors several integration paths.
                     (handler-case (sleep 2)
                       (error () :caught))))
             (let* ((response
                      (cl-tron-mcp/protocol:parse-message
                       (cl-tron-mcp/tools::execute-tool-with-timeout
                        "timeout-test" nil 93)))
                    (error-object (getf response :|error|)))
               (ok (= -32008 (getf error-object :|code|)))
               (ok (search "timeout" (getf error-object :|message|)
                           :test #'char-equal))))
        (setf (symbol-function 'cl-tron-mcp/tools:call-tool)
              original-call-tool)))))

(deftest outer-tool-timeout-allows-inner-cleanup-test
  (testing "a tool's own timeout expires before the outer execution deadline"
    (let ((cl-tron-mcp/tools:*default-tool-timeout* 300)
          (cl-tron-mcp/tools::*tool-timeout-cleanup-grace* 5))
      (ok (= 305 (cl-tron-mcp/tools::requested-tool-timeout nil)))
      (ok (= 305
             (cl-tron-mcp/tools::requested-tool-timeout
              (list :|timeout| 120))))
      (ok (= 905
             (cl-tron-mcp/tools::requested-tool-timeout
              (list :|timeout| 900))))
      (ok (= 3605
             (cl-tron-mcp/tools::requested-tool-timeout
              (list :|timeout| (expt 10 100))))))))

(deftest oversized-tool-timeout-reaches-schema-validation-test
  (testing "an oversized wire timeout cannot overflow the outer deadline timer"
    (let* ((response
             (cl-tron-mcp/protocol:parse-message
              (cl-tron-mcp/tools::execute-tool-with-timeout
               "repl_eval"
               (list :|code| "(+ 1 2)" :|timeout| (expt 10 100))
               94)))
           (text (getf (first (getf (getf response :|result|) :|content|))
                       :|text|)))
      (ok (search "maximum 3600" text))
      (ok (not (search "SIGNED-BYTE" text :test #'char-equal))))))
