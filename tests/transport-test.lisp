;;;; tests/transport-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest http-response-test
    (testing "HTTP response is formatted correctly"
             (let ((response (cl-tron-mcp/transport:http-ok "Hello World" "application/json")))
               (ok (stringp response))
               (ok (search "200 OK" response))
               (ok (search "Content-Type: application/json" response))
               (ok (search "Content-Length:" response)))))

(deftest http-not-found-test
    (testing "HTTP 404 response is formatted correctly"
             (let ((response (cl-tron-mcp/transport:http-not-found)))
               (ok (stringp response))
               (ok (search "404 Not Found" response)))))

(deftest http-transport-config-test
    (testing "HTTP transport configuration variables are set"
             (ok (integerp cl-tron-mcp/transport:*max-concurrent-connections*))
             (ok (integerp cl-tron-mcp/transport:*http-request-timeout*))
             (ok (or (eq cl-tron-mcp/transport:*rate-limit-enabled* t)
                     (eq cl-tron-mcp/transport:*rate-limit-enabled* nil)))
             (ok (integerp cl-tron-mcp/transport:*rate-limit-requests-per-minute*))
             (ok (integerp cl-tron-mcp/transport:*max-request-size*))
             (ok (integerp cl-tron-mcp/transport:*http-connection-timeout*))
             (ok (> cl-tron-mcp/transport:*max-concurrent-connections* 0))
             (ok (> cl-tron-mcp/transport:*http-request-timeout* 0))
             (ok (> cl-tron-mcp/transport:*rate-limit-requests-per-minute* 0))
             (ok (> cl-tron-mcp/transport:*max-request-size* 0))
             (ok (> cl-tron-mcp/transport:*http-connection-timeout* 0))))

(deftest http-transport-default-values-test
    (testing "HTTP transport has sensible default values"
             (ok (= cl-tron-mcp/transport:*max-concurrent-connections* 100))
             (ok (= cl-tron-mcp/transport:*http-request-timeout* 30))
             (ok cl-tron-mcp/transport:*rate-limit-enabled*)
             (ok (= cl-tron-mcp/transport:*rate-limit-requests-per-minute* 60))
             (ok (= cl-tron-mcp/transport:*max-request-size* (* 10 1024 1024)))
             (ok (= cl-tron-mcp/transport:*http-connection-timeout* 10))))

(deftest stdio-keeps-diagnostics-out-of-protocol-test
  (testing "stdio reserves stdout for JSON even when a handler resets logging"
    (let ((protocol-output (make-string-output-stream))
          (diagnostic-output (make-string-output-stream))
          (saved-error *error-output*)
          (response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"))
      (unwind-protect
           (let ((*standard-input*
                   (make-string-input-stream
                    (format nil
                            "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}~%")))
                 (*standard-output* protocol-output)
                 (*error-output* diagnostic-output))
             (cl-tron-mcp/transport:start-stdio-transport
              :handler (lambda (message)
                         (declare (ignore message))
                         ;; Simulate both ordinary library output and a logger
                         ;; that has discarded Tron's configured appender.
                         (format t "ordinary diagnostic~%")
                         (log4cl:clear-logging-configuration)
                         (log:config :sane :info)
                         (cl-tron-mcp/logging:log-info "logger diagnostic")
                         (bordeaux-threads:join-thread
                          (cl-tron-mcp/logging:make-diagnostic-thread
                           (lambda ()
                             (cl-tron-mcp/logging:log-info
                              "background logger diagnostic"))
                           :name "test-diagnostic-logger"))
                         response)))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error))
      (let ((protocol-text (get-output-stream-string protocol-output))
            (diagnostic-text (get-output-stream-string diagnostic-output)))
        (ok (string= protocol-text (format nil "~a~%" response)))
        (ok (search "ordinary diagnostic" diagnostic-text))
        (ok (search "logger diagnostic" diagnostic-text))
        (ok (search "background logger diagnostic" diagnostic-text))))))

(deftest stdio-handler-cannot-consume-protocol-input-test
  (testing "handler reads see EOF while the protocol loop retains later requests"
    (let ((protocol-output (make-string-output-stream))
          (diagnostic-output (make-string-output-stream))
          (saved-error *error-output*)
          (handler-read :unset)
          (calls 0))
      (unwind-protect
           (let ((*standard-input*
                   (make-string-input-stream
                    (format nil
                            "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}~%{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}~%")))
                 (*standard-output* protocol-output)
                 (*error-output* diagnostic-output))
             (cl-tron-mcp/transport:start-stdio-transport
              :handler
              (lambda (message)
                (declare (ignore message))
                (incf calls)
                (when (= calls 1)
                  (setf handler-read (read-line *standard-input* nil)))
                (format nil
                        "{\"jsonrpc\":\"2.0\",\"id\":~d,\"result\":{}}"
                        calls))))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error))
      (ok (null handler-read))
      (ok (= 2 calls))
      (ok (string=
           (get-output-stream-string protocol-output)
           (format nil
                   "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}~%{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{}}~%"))))))

(deftest malformed-stdio-json-returns-parse-error-test
  (testing "stdio returns JSON-RPC -32700 with a null request id"
    (let ((protocol-output (make-string-output-stream))
          (diagnostic-output (make-string-output-stream))
          (saved-error *error-output*))
      (unwind-protect
           (let ((*standard-input* (make-string-input-stream "{bad\n"))
                 (*standard-output* protocol-output)
                 (*error-output* diagnostic-output))
             (cl-tron-mcp/transport:start-stdio-transport))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error))
      (let* ((response
               (cl-tron-mcp/protocol:parse-message
                (string-trim '(#\Newline #\Return)
                             (get-output-stream-string protocol-output))))
             (error-object (getf response :|error|)))
        (ok (= -32700 (getf error-object :|code|)))
        (ok (eq :null (getf response :|id|)))))))

(deftest stdio-custom-handler-receives-decoded-message-test
  (testing "stdio preserves the decoded-message handler contract"
    (let ((protocol-output (make-string-output-stream))
          (diagnostic-output (make-string-output-stream))
          (saved-error *error-output*)
          (received nil))
      (unwind-protect
	           (let ((*standard-input*
	                   (make-string-input-stream
	                    (format nil
	                            "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"ping\"}~%")))
                 (*standard-output* protocol-output)
                 (*error-output* diagnostic-output))
             (cl-tron-mcp/transport:start-stdio-transport
              :handler (lambda (message)
                         (setf received message)
                         nil)))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error))
      (ok (listp received))
      (ok (= 7 (getf received :|id|)))
      (ok (string= "ping" (getf received :|method|))))))

(deftest stop-stdio-transport-wakes-idle-reader-test
  (testing "combined-mode shutdown wakes a stdio thread blocked on input"
    (let* ((process
             (uiop:launch-program '("sh" "-c" "sleep 10")
                                  :output :stream
                                  :error-output :stream))
           (input (uiop:process-info-output process))
           (diagnostic-output (make-string-output-stream))
           (protocol-output (make-string-output-stream))
           (saved-error *error-output*)
           (thread
             (bordeaux-threads:make-thread
              (lambda ()
                (let ((*standard-input* input)
                      (*standard-output* protocol-output)
                      (*error-output* diagnostic-output))
                  (cl-tron-mcp/transport:start-stdio-transport)))
              :name "test-idle-stdio")))
      (unwind-protect
           (progn
             (loop repeat 200
                   until (eq cl-tron-mcp/transport::*stdio-thread* thread)
                   do (sleep 0.01))
             (ok (eq cl-tron-mcp/transport::*stdio-thread* thread))
             (cl-tron-mcp/transport:stop-stdio-transport)
             (loop repeat 100
                   while (bordeaux-threads:thread-alive-p thread)
                   do (sleep 0.01))
             (ok (not (bordeaux-threads:thread-alive-p thread)))
             (ok (open-stream-p input))
             (let ((restart-thread
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (let ((*standard-input* input)
                              (*standard-output* protocol-output)
                              (*error-output* diagnostic-output))
                          (cl-tron-mcp/transport:start-stdio-transport)))
                      :name "test-restarted-idle-stdio")))
               (loop repeat 200
                     until (eq cl-tron-mcp/transport::*stdio-thread*
                               restart-thread)
                     do (sleep 0.01))
               (ok (eq cl-tron-mcp/transport::*stdio-thread* restart-thread))
               (cl-tron-mcp/transport:stop-stdio-transport)
               (loop repeat 100
                     while (bordeaux-threads:thread-alive-p restart-thread)
                     do (sleep 0.01))
               (ok (not (bordeaux-threads:thread-alive-p restart-thread)))
               (ok (open-stream-p input))))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error)
        (when (uiop:process-alive-p process)
          (uiop:terminate-process process))
        (ignore-errors (close input))))))

(deftest replaced-stdio-owner-cannot-resume-or-emit-test
  (testing "a stopped busy handler cannot interfere with its replacement"
    (let ((gate (bordeaux-threads:make-lock "test-stdio-replacement"))
          (condition (bordeaux-threads:make-condition-variable))
          (entered-p nil)
          (release-p nil)
          (first-output (make-string-output-stream))
          (second-output (make-string-output-stream))
          (first-diagnostics (make-string-output-stream))
          (second-diagnostics (make-string-output-stream))
          (saved-error *error-output*)
          first-thread
          second-thread)
      (unwind-protect
           (progn
             (setf first-thread
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((*standard-input*
                              (make-string-input-stream
                               (format nil
                                       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}~%")))
                            (*standard-output* first-output)
                            (*error-output* first-diagnostics))
                        (cl-tron-mcp/transport:start-stdio-transport
                         :handler
                         (lambda (message)
                           (declare (ignore message))
                           (bordeaux-threads:with-lock-held (gate)
                             (setf entered-p t)
                             (bordeaux-threads:condition-notify condition)
                             (loop until release-p
                                   do (bordeaux-threads:condition-wait
                                       condition gate)))
                           "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"))))
                    :name "test-busy-stdio-owner"))
             (bordeaux-threads:with-lock-held (gate)
               (loop until entered-p
                     do (bordeaux-threads:condition-wait condition gate)))
             (cl-tron-mcp/transport:stop-stdio-transport)
             (setf second-thread
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((*standard-input*
                              (make-string-input-stream
                               (format nil
                                       "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}~%")))
                            (*standard-output* second-output)
                            (*error-output* second-diagnostics))
                        (cl-tron-mcp/transport:start-stdio-transport
                         :handler
                         (lambda (message)
                           (declare (ignore message))
                           "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{}}"))))
                    :name "test-replacement-stdio-owner"))
             (bordeaux-threads:join-thread second-thread)
             (bordeaux-threads:with-lock-held (gate)
               (setf release-p t)
               (bordeaux-threads:condition-notify condition))
             (bordeaux-threads:join-thread first-thread)
             (ok (zerop (length (get-output-stream-string first-output))))
             (ok (string=
                  (get-output-stream-string second-output)
                  (format nil
                          "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{}}~%"))))
        (bordeaux-threads:with-lock-held (gate)
          (setf release-p t)
          (bordeaux-threads:condition-notify condition))
        (cl-tron-mcp/transport:stop-stdio-transport)
        (dolist (thread (list first-thread second-thread))
          (when (and thread (bordeaux-threads:thread-alive-p thread))
            (ignore-errors (bordeaux-threads:join-thread thread))))
        (cl-tron-mcp/logging:ensure-log-to-stream saved-error)))))

(deftest stopped-pending-stdio-session-cannot-start-test
  (testing "stop invalidates a combined reader before its thread enters start"
    (let ((token (cl-tron-mcp/transport::reserve-stdio-session))
          (calls 0)
          (protocol-output (make-string-output-stream))
          result)
      (cl-tron-mcp/transport:stop-stdio-transport)
      (let ((*standard-input*
              (make-string-input-stream
               (format nil
                       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}~%")))
            (*standard-output* protocol-output))
        (setf result
              (cl-tron-mcp/transport:start-stdio-transport
               :session-token token
               :handler (lambda (message)
                          (declare (ignore message))
                          (incf calls)))))
      (ok (null result))
      (ok (zerop calls))
      (ok (zerop (length (get-output-stream-string protocol-output)))))))

(deftest http-handler-keeps-diagnostics-out-of-stdio-test
  (testing "HTTP tool output cannot enter a simultaneous stdio protocol stream"
    (let ((original-handler
            (symbol-function 'cl-tron-mcp/protocol:handle-message))
          (protocol-output (make-string-output-stream))
          (diagnostic-output (make-string-output-stream))
          (response nil)
          (status nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/protocol:handle-message)
                   (lambda (message)
                     (declare (ignore message))
                     (format t "http diagnostic")
                     "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"))
             (let ((*standard-output* protocol-output)
                   (*error-output* diagnostic-output))
               (setf (values response status)
                     (cl-tron-mcp/transport::handle-rpc-body
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")))
             (ok (= 200 status))
             (ok (search "\"id\":1" response))
             (ok (zerop (length (get-output-stream-string protocol-output))))
             (ok (search "http diagnostic"
                         (get-output-stream-string diagnostic-output))))
        (setf (symbol-function 'cl-tron-mcp/protocol:handle-message)
              original-handler)))))
