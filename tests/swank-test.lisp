;;;; tests/swank-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest swank-connection-state-test
    (testing "Swank connection state variables exist"
             (ok (boundp 'cl-tron-mcp/swank::*swank-connected*))
             (ok (boundp 'cl-tron-mcp/swank::*swank-socket*))
             (ok (boundp 'cl-tron-mcp/swank::*swank-io*))))

(deftest repl-connect-returns-flat-property-list-test
  (testing "the unified connection result remains a valid property list"
    (let ((original-connect
            (symbol-function 'cl-tron-mcp/swank:swank-connect))
          (cl-tron-mcp/unified::*repl-connected* nil)
          (cl-tron-mcp/unified::*repl-type* nil)
          (cl-tron-mcp/unified::*repl-port* nil)
          (cl-tron-mcp/unified::*repl-host* nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (list :success t :host "127.0.0.1" :port 4005)))
             (let ((result
                     (cl-tron-mcp/unified:repl-connect
                      :type :swank :host "127.0.0.1" :port 4005)))
               (ok (getf result :success))
               (ok (eq :swank (getf result :type)))
               (ok (= 4005 (getf result :port)))))
        (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
              original-connect)))))

(deftest repl-status-clears-invalidated-swank-state-test
  (testing "unified status reflects reader-side Swank invalidation"
    (let ((original-connected-p
            (symbol-function 'cl-tron-mcp/swank:swank-connected-p))
          (cl-tron-mcp/unified::*repl-connected* t)
          (cl-tron-mcp/unified::*repl-type* :swank)
          (cl-tron-mcp/unified::*repl-port* 4006)
          (cl-tron-mcp/unified::*repl-host* "127.0.0.1"))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank:swank-connected-p)
                   (lambda () nil))
             (let ((status (cl-tron-mcp/unified:repl-status)))
               (ok (null (getf status :connected)))
               (ok (null (getf status :type)))
               (ok (null cl-tron-mcp/unified::*repl-connected*))))
        (setf (symbol-function 'cl-tron-mcp/swank:swank-connected-p)
              original-connected-p)))))

(deftest raw-swank-connect-preserves-callee-defaults-test
  (testing "omitted MCP host and port remain omitted at the Swank API"
    (let ((original-connect
            (symbol-function 'cl-tron-mcp/swank:swank-connect))
          (arguments :unset))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
                   (lambda (&rest values)
                     (setf arguments values)
                     (list :success t)))
             (ok (getf (cl-tron-mcp/tools:call-tool "swank_connect" nil)
                       :success))
             (ok (null arguments)))
        (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
              original-connect)))))

(deftest swank-error-handling-test
    (testing "Swank functions return errors when not connected"
             ;; These should return error responses
             (let ((result (cl-tron-mcp/swank:mcp-swank-threads)))
               (ok (getf result :error)))
             (let ((result (cl-tron-mcp/swank:mcp-swank-backtrace)))
               (ok (getf result :error)))
             (let ((result (cl-tron-mcp/swank:mcp-swank-completions :prefix "mak")))
               (ok (getf result :error)))))

(deftest swank-debugger-state-tool-test
    (testing "swank_debugger_state returns structured debugger state"
             (let ((cl-tron-mcp/swank::*debugger-thread* 123)
                  (cl-tron-mcp/swank::*debugger-level* 2))
              (let ((result (cl-tron-mcp/tools:call-tool "swank_debugger_state" nil)))
                (ok (listp result))
                (ok (= 123 (getf result :thread)))
                (ok (= 2 (getf result :level)))
                (ok (getf result :in-debugger))))))

(deftest debug-only-event-queue-waits-test
  (testing "the event processor waits when only debugger events are queued"
    (let ((cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-swank-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*event-processor-running* t))
      (cl-tron-mcp/swank::enqueue-debugger-event
       "test condition" '("ABORT") '((0 "frame")))
      (let ((start (get-internal-real-time)))
        (ok (null (cl-tron-mcp/swank::dequeue-event 0.05)))
        (ok (>= (/ (- (get-internal-real-time) start)
                   (float internal-time-units-per-second))
                0.04)))
      (ok (= 1 (length cl-tron-mcp/swank::*event-queue*)))
      (ok (eq :debug
              (cl-tron-mcp/swank::swank-event-type
               (aref cl-tron-mcp/swank::*event-queue* 0)))))))

(deftest bounded-event-queue-enqueue-test
  (testing "enqueue reserves space without recursively acquiring the event lock"
    (let ((cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-bounded-swank-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*max-event-queue-size* 2))
      (cl-tron-mcp/swank::enqueue-output-event "one" nil)
      (cl-tron-mcp/swank::enqueue-output-event "two" nil)
      (cl-tron-mcp/swank::enqueue-output-event "three" nil)
      (ok (= 2 (length cl-tron-mcp/swank::*event-queue*)))
      (ok (equal '("two" "three")
                 (loop for event across cl-tron-mcp/swank::*event-queue*
                       collect (getf (cl-tron-mcp/swank::swank-event-data event)
                                     :string)))))))

(deftest synchronous-swank-request-cleans-correlation-state-test
  (testing "a completed request is removed and no longer current"
    (let ((original-write-message
            (symbol-function 'cl-tron-mcp/swank::write-message))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-swank-connection"))
          (cl-tron-mcp/swank::*pending-requests* (make-hash-table :test 'eql))
          (cl-tron-mcp/swank::*request-lock*
            (bordeaux-threads:make-lock "test-swank-requests"))
          (cl-tron-mcp/swank::*current-request-id* nil)
          (cl-tron-mcp/swank::*next-request-id* 1)
          (cl-tron-mcp/swank::*connection-generation* 7)
          (written-generation nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank::write-message)
                   (lambda (message &key expected-generation)
                     (setf written-generation expected-generation)
                     (cl-tron-mcp/swank::fulfill-request
                      (fifth message) '(:result (:ok t)))))
             (ok (equal '(:result (:ok t))
                        (cl-tron-mcp/swank::send-request '(test))))
             (ok (zerop (hash-table-count
                         cl-tron-mcp/swank::*pending-requests*)))
             (ok (null cl-tron-mcp/swank::*current-request-id*))
             (ok (= 7 written-generation)))
        (setf (symbol-function 'cl-tron-mcp/swank::write-message)
              original-write-message)))))

(deftest debugger-event-uses-swank-continuation-id-test
  (testing "a late debugger event cannot complete a newer request"
    (let* ((old-request
             (cl-tron-mcp/swank::make-swank-request
              :id 1
              :condition (bordeaux-threads:make-condition-variable)
              :completed-p nil))
           (new-request
             (cl-tron-mcp/swank::make-swank-request
              :id 2
              :condition (bordeaux-threads:make-condition-variable)
              :completed-p nil))
           (cl-tron-mcp/swank::*pending-requests* (make-hash-table :test 'eql))
           (cl-tron-mcp/swank::*request-lock*
             (bordeaux-threads:make-lock "test-swank-debug-correlation"))
           (cl-tron-mcp/swank::*current-request-id* 2)
           (cl-tron-mcp/swank::*event-queue*
             (make-array 2 :adjustable t :fill-pointer 0))
           (cl-tron-mcp/swank::*event-mutex*
             (bordeaux-threads:make-lock "test-swank-debug-event"))
           (cl-tron-mcp/swank::*event-condition*
             (bordeaux-threads:make-condition-variable))
           (cl-tron-mcp/swank::*debugger-thread* nil)
           (cl-tron-mcp/swank::*debugger-level* 0))
      (setf (gethash 1 cl-tron-mcp/swank::*pending-requests*) old-request
            (gethash 2 cl-tron-mcp/swank::*pending-requests*) new-request)
      (cl-tron-mcp/swank::dispatch-incoming-message
       '(:debug 17 1 ("old failure" "condition") nil nil (1)))
      (ok (cl-tron-mcp/swank::swank-request-completed-p old-request))
      (ok (not (cl-tron-mcp/swank::swank-request-completed-p new-request)))
      (ok (search "old failure"
                  (prin1-to-string
                   (cl-tron-mcp/swank::swank-request-response old-request)))))))

(deftest swank-packet-writes-are-serialized-test
  (testing "concurrent writers cannot interleave Swank packet frames"
    (let ((original-protocol-writer
            (symbol-function 'cl-tron-mcp/swank-protocol:write-message))
          (original-write-lock
            (symbol-value 'cl-tron-mcp/swank::*write-lock*))
          (original-connection-lock
            (symbol-value 'cl-tron-mcp/swank::*connection-lock*))
          (original-connected
            (symbol-value 'cl-tron-mcp/swank::*swank-connected*))
          (original-running
            (symbol-value 'cl-tron-mcp/swank::*swank-running*))
          (original-io (symbol-value 'cl-tron-mcp/swank::*swank-io*))
          (original-socket (symbol-value 'cl-tron-mcp/swank::*swank-socket*))
          (original-generation
            (symbol-value 'cl-tron-mcp/swank::*connection-generation*))
          (counter-lock (bordeaux-threads:make-lock "test-write-counter"))
          (active-writers 0)
          (maximum-writers 0))
      (unwind-protect
           (progn
             (setf (symbol-value 'cl-tron-mcp/swank::*write-lock*)
                   (bordeaux-threads:make-lock "test-swank-writes")
                   (symbol-value 'cl-tron-mcp/swank::*connection-lock*)
                   (bordeaux-threads:make-lock "test-swank-connection")
                   (symbol-value 'cl-tron-mcp/swank::*swank-connected*) t
                   (symbol-value 'cl-tron-mcp/swank::*swank-running*) t
                   (symbol-value 'cl-tron-mcp/swank::*swank-io*) t
                   (symbol-value 'cl-tron-mcp/swank::*swank-socket*) t
                   (symbol-value 'cl-tron-mcp/swank::*connection-generation*) 1
                   (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (bordeaux-threads:with-lock-held (counter-lock)
                       (incf active-writers)
                       (setf maximum-writers
                             (max maximum-writers active-writers)))
                     (sleep 0.02)
                     (bordeaux-threads:with-lock-held (counter-lock)
                       (decf active-writers))))
             (let ((threads
                     (loop repeat 4
                           collect
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (cl-tron-mcp/swank::write-message '(:ping)))))))
               (dolist (thread threads)
                 (bordeaux-threads:join-thread thread)))
             (ok (= 1 maximum-writers)))
        (setf (symbol-value 'cl-tron-mcp/swank::*write-lock*)
              original-write-lock
              (symbol-value 'cl-tron-mcp/swank::*connection-lock*)
              original-connection-lock
              (symbol-value 'cl-tron-mcp/swank::*swank-connected*)
              original-connected
              (symbol-value 'cl-tron-mcp/swank::*swank-running*)
              original-running
              (symbol-value 'cl-tron-mcp/swank::*swank-io*) original-io
              (symbol-value 'cl-tron-mcp/swank::*swank-socket*) original-socket
              (symbol-value 'cl-tron-mcp/swank::*connection-generation*)
              original-generation
              (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
              original-protocol-writer)))))

(deftest interrupted-swank-write-invalidates-connection-test
  (testing "a packet write deadline is prompt and the partial stream is discarded"
    (let ((original-protocol-writer
            (symbol-function 'cl-tron-mcp/swank-protocol:write-message))
          (cl-tron-mcp/swank::*write-lock*
            (bordeaux-threads:make-lock "test-interrupted-swank-write"))
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-interrupted-swank-connection"))
          (cl-tron-mcp/swank::*connection-generation* 0)
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-interrupted-swank-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*event-processor-running* t)
          (cl-tron-mcp/swank::*heartbeat-running* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (start (get-internal-real-time)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (sleep 1)))
             (handler-case
                 (bordeaux-threads:with-timeout (0.05)
                   (cl-tron-mcp/swank::write-message '(:ping)))
               (bordeaux-threads:timeout ()))
             (ok (< (/ (- (get-internal-real-time) start)
                       (float internal-time-units-per-second))
                    0.5))
             (ok (not cl-tron-mcp/swank::*swank-connected*))
             (ok (null cl-tron-mcp/swank::*swank-io*))
             (ok (null cl-tron-mcp/swank::*swank-socket*)))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
              original-protocol-writer)))))

(deftest queued-swank-write-timeout-preserves-connection-test
  (testing "timing out before acquiring the write lock does not invalidate a stream"
    (let* ((write-lock
             (bordeaux-threads:make-lock "test-queued-swank-write"))
           (cl-tron-mcp/swank::*write-lock* write-lock)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-queued-swank-connection"))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (cl-tron-mcp/swank::*connection-generation* 1)
          (holder-ready nil)
          (holder
            (bordeaux-threads:make-thread
             (lambda ()
               (bordeaux-threads:with-lock-held (write-lock)
                 (setf holder-ready t)
                 (sleep 0.2)))
             :name "test-swank-write-holder")))
      (loop until holder-ready do (sleep 0.001))
      (unwind-protect
           (handler-case
               (bordeaux-threads:with-timeout (0.05)
                 (cl-tron-mcp/swank::write-message '(:ping)))
             (bordeaux-threads:timeout ()))
        (bordeaux-threads:join-thread holder))
      (ok cl-tron-mcp/swank::*swank-connected*)
      (ok (eq t cl-tron-mcp/swank::*swank-io*))
      (ok (= 1 cl-tron-mcp/swank::*connection-generation*)))))

(deftest stale-swank-writer-cannot-invalidate-replacement-test
  (testing "failed cleanup is conditional on the connection it wrote"
    (let ((original-protocol-writer
            (symbol-function 'cl-tron-mcp/swank-protocol:write-message))
          (old-io (gensym "OLD-IO"))
          (old-socket (gensym "OLD-SOCKET"))
          (new-io (gensym "NEW-IO"))
          (new-socket (gensym "NEW-SOCKET"))
          (cl-tron-mcp/swank::*write-lock*
            (bordeaux-threads:make-lock "test-stale-swank-write"))
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-stale-swank-connection"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-stale-swank-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*connection-generation* 1))
      (setf cl-tron-mcp/swank::*swank-io* old-io
            cl-tron-mcp/swank::*swank-socket* old-socket)
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
                   (lambda (message package stream)
                     (declare (ignore message package))
                     (ok (eq old-io stream))
                     (setf cl-tron-mcp/swank::*swank-io* new-io
                           cl-tron-mcp/swank::*swank-socket* new-socket
                           cl-tron-mcp/swank::*connection-generation* 2)
                     (error "old write failed")))
             (handler-case
                 (cl-tron-mcp/swank::write-message '(:ping))
               (error ()))
             (ok cl-tron-mcp/swank::*swank-connected*)
             (ok (eq new-io cl-tron-mcp/swank::*swank-io*))
             (ok (eq new-socket cl-tron-mcp/swank::*swank-socket*))
             (ok (= 2 cl-tron-mcp/swank::*connection-generation*)))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
              original-protocol-writer)))))

(deftest swank-protocol-write-preserves-timeout-condition-test
  (testing "the wire encoder does not mask an interrupting deadline"
    (let ((original-write-header
            (symbol-function 'cl-tron-mcp/swank-protocol::write-header)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank-protocol::write-header)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (sleep 1)))
             (let ((condition
                     (handler-case
                         (bordeaux-threads:with-timeout (0.05)
                           (cl-tron-mcp/swank-protocol:write-message
                            '(:ping) (find-package :cl)
                            (make-broadcast-stream)))
                       (bordeaux-threads:timeout (condition) condition)
                       (condition (condition) condition))))
               (ok (typep condition 'bordeaux-threads:timeout))))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol::write-header)
              original-write-header)))))

(deftest stale-swank-reader-does-not-dispatch-test
  (testing "a packet read after generation change is discarded"
    (let ((original-read-packet
            (symbol-function 'cl-tron-mcp/swank-protocol:read-packet))
          (original-dispatch
            (symbol-function 'cl-tron-mcp/swank::dispatch-incoming-message))
          (old-io (gensym "OLD-READER-IO"))
          (new-io (gensym "NEW-READER-IO"))
          (dispatched-p nil)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*connection-generation* 1)
          (cl-tron-mcp/swank::*swank-io-package* (find-package :cl)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank-protocol:read-packet)
                   (lambda (stream &rest arguments)
                     (declare (ignore arguments))
                     (ok (eq old-io stream))
                     (setf cl-tron-mcp/swank::*connection-generation* 2
                           cl-tron-mcp/swank::*swank-io* new-io)
                     "(:return (:ok :old) 1)")
                   (symbol-function
                    'cl-tron-mcp/swank::dispatch-incoming-message)
                   (lambda (message)
                     (declare (ignore message))
                     (setf dispatched-p t)))
             (cl-tron-mcp/swank::swank-reader-loop 1 old-io nil)
             (ok (not dispatched-p)))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol:read-packet)
              original-read-packet
              (symbol-function 'cl-tron-mcp/swank::dispatch-incoming-message)
              original-dispatch)))))

(deftest fatal-swank-reader-invalidates-before-reconnect-test
  (testing "EOF detaches the dead connection before automatic reconnect"
    (let ((original-read-packet
            (symbol-function 'cl-tron-mcp/swank-protocol:read-packet))
          (original-reconnect
            (symbol-function 'cl-tron-mcp/swank::start-reconnect-worker))
          (old-io (gensym "FAILED-READER-IO"))
          (old-socket (gensym "FAILED-READER-SOCKET"))
          (reconnect-generation nil)
          (reconnect-host nil)
          (reconnect-port nil)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-failed-reader-connection"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-failed-reader-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*swank-host* "reader.example")
          (cl-tron-mcp/swank::*swank-port* 4999)
          (cl-tron-mcp/swank::*event-processor-running* t)
          (cl-tron-mcp/swank::*heartbeat-running* t)
          (cl-tron-mcp/swank::*connection-generation* 1)
          (cl-tron-mcp/swank::*reconnect-enabled* t))
      (setf cl-tron-mcp/swank::*swank-io* old-io
            cl-tron-mcp/swank::*swank-socket* old-socket)
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank-protocol:read-packet)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error 'end-of-file))
                   (symbol-function 'cl-tron-mcp/swank::start-reconnect-worker)
                   (lambda (&key expected-generation host port)
                     (setf reconnect-generation expected-generation
                           reconnect-host host
                           reconnect-port port)
                     (list :success t)))
             (cl-tron-mcp/swank::swank-reader-loop 1 old-io old-socket)
             (ok (not cl-tron-mcp/swank::*swank-connected*))
             (ok (null cl-tron-mcp/swank::*swank-io*))
             (ok (null cl-tron-mcp/swank::*swank-socket*))
             (ok (= 2 reconnect-generation))
             (ok (string= "reader.example" reconnect-host))
             (ok (= 4999 reconnect-port)))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol:read-packet)
              original-read-packet
              (symbol-function 'cl-tron-mcp/swank::start-reconnect-worker)
              original-reconnect)))))

(deftest automatic-reconnect-retries-until-success-test
  (testing "automatic reconnect consumes its bounded retry budget"
    (let ((original-connect
            (symbol-function 'cl-tron-mcp/swank:swank-connect))
          (calls 0)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-reconnect-retry"))
          (cl-tron-mcp/swank::*swank-connected* nil)
          (cl-tron-mcp/swank::*connection-generation* 7)
          (cl-tron-mcp/swank::*reconnect-enabled* t)
          (cl-tron-mcp/swank::*reconnect-delay* 0)
          (cl-tron-mcp/swank::*reconnect-max-attempts* 3)
          (cl-tron-mcp/swank::*reconnect-attempt-count* 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf calls)
                     (if (< calls 3)
                         (list :error t :message "not yet")
                         (list :success t))))
             (let ((result
                     (cl-tron-mcp/swank:attempt-reconnect
                      :expected-generation 7)))
               (ok (getf result :success))
               (ok (= 3 calls))
               (ok (zerop cl-tron-mcp/swank::*reconnect-attempt-count*))))
        (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
              original-connect)))))

(deftest superseded-reconnect-does-not-consume-retry-budget-test
  (testing "a stale reconnect worker cannot alter the next connection's budget"
    (let ((original-connect
            (symbol-function 'cl-tron-mcp/swank:swank-connect))
          (calls 0)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-reconnect-superseded"))
          (cl-tron-mcp/swank::*swank-connected* nil)
          (cl-tron-mcp/swank::*connection-generation* 7)
          (cl-tron-mcp/swank::*reconnect-enabled* t)
          (cl-tron-mcp/swank::*reconnect-delay* 0)
          (cl-tron-mcp/swank::*reconnect-max-attempts* 3)
          (cl-tron-mcp/swank::*reconnect-attempt-count* 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf calls)
                     ;; Model a manual connection superseding this worker
                     ;; immediately after its first failed attempt.
                     (setf cl-tron-mcp/swank::*connection-generation* 8
                           cl-tron-mcp/swank::*reconnect-attempt-count* 0)
                     (list :error t :message "superseded")))
             (let ((result
                     (cl-tron-mcp/swank:attempt-reconnect
                      :expected-generation 7)))
               (ok (getf result :cancelled))
               (ok (= 1 calls))
               (ok (zerop cl-tron-mcp/swank::*reconnect-attempt-count*))))
        (setf (symbol-function 'cl-tron-mcp/swank:swank-connect)
              original-connect)))))

(deftest reconnect-worker-deduplicates-by-generation-test
  (testing "a stale live worker cannot suppress recovery for a newer failure"
    (let ((original-thread-maker
            (symbol-function 'cl-tron-mcp/logging:make-diagnostic-thread))
          (original-thread-alive
            (symbol-function 'bordeaux-threads:thread-alive-p))
          (created 0)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-worker-generation"))
          (cl-tron-mcp/swank::*connection-generation* 8)
          (cl-tron-mcp/swank::*reconnect-thread* :stale-worker)
          (cl-tron-mcp/swank::*reconnect-generation* 7))
      (unwind-protect
           (progn
             (setf (symbol-function
                    'cl-tron-mcp/logging:make-diagnostic-thread)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf created)
                     :new-worker)
                   (symbol-function 'bordeaux-threads:thread-alive-p)
                   (lambda (thread)
                     (declare (ignore thread))
                     t))
             (ok (getf (cl-tron-mcp/swank::start-reconnect-worker
                        :expected-generation 8
                        :host "new.example"
                        :port 4555)
                       :scheduled))
             (ok (= 1 created))
             (ok (eq :new-worker cl-tron-mcp/swank::*reconnect-thread*))
             (ok (= 8 cl-tron-mcp/swank::*reconnect-generation*))
             (ok (getf (cl-tron-mcp/swank::start-reconnect-worker
                        :expected-generation 8
                        :host "new.example"
                        :port 4555)
                       :already-running))
             (ok (= 1 created)))
        (setf (symbol-function
               'cl-tron-mcp/logging:make-diagnostic-thread)
              original-thread-maker
              (symbol-function 'bordeaux-threads:thread-alive-p)
              original-thread-alive)))))

(deftest invalidation-clears-connection-owned-session-state-test
  (testing "a replacement connection cannot inherit debugger or input IDs"
    (let ((cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-state-reset-connection"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-state-reset-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*input-request-lock*
            (bordeaux-threads:make-lock "test-state-reset-input"))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*debugger-thread* 17)
          (cl-tron-mcp/swank::*debugger-level* 1)
          (cl-tron-mcp/swank::*pending-input-requests* '((17 . 3)))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*event-processor-running* t)
          (cl-tron-mcp/swank::*heartbeat-running* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (cl-tron-mcp/swank::*connection-generation* 4))
      (cl-tron-mcp/swank::enqueue-debugger-event "old" nil nil)
      (cl-tron-mcp/swank::invalidate-swank-connection
       :expected-io t :expected-socket t :expected-generation 4)
      (ok (zerop (length cl-tron-mcp/swank::*event-queue*)))
      (ok (null cl-tron-mcp/swank::*debugger-thread*))
      (ok (zerop cl-tron-mcp/swank::*debugger-level*))
      (ok (null cl-tron-mcp/swank::*pending-input-requests*)))))

(deftest invalidation-wakes-pending-request-test
  (testing "a detached connection completes waiters immediately"
    (let* ((request
             (cl-tron-mcp/swank::make-swank-request
              :id 31
              :condition (bordeaux-threads:make-condition-variable)
              :completed-p nil))
           (cl-tron-mcp/swank::*connection-lock*
             (bordeaux-threads:make-lock "test-wake-connection"))
           (cl-tron-mcp/swank::*event-mutex*
             (bordeaux-threads:make-lock "test-wake-events"))
           (cl-tron-mcp/swank::*event-condition*
             (bordeaux-threads:make-condition-variable))
           (cl-tron-mcp/swank::*input-request-lock*
             (bordeaux-threads:make-lock "test-wake-input"))
           (cl-tron-mcp/swank::*request-lock*
             (bordeaux-threads:make-lock "test-wake-request"))
           (cl-tron-mcp/swank::*event-queue*
             (make-array 2 :adjustable t :fill-pointer 0))
           (cl-tron-mcp/swank::*pending-requests*
             (make-hash-table :test 'eql))
           (cl-tron-mcp/swank::*current-request-id* 31)
           (cl-tron-mcp/swank::*swank-connected* t)
           (cl-tron-mcp/swank::*swank-running* t)
           (cl-tron-mcp/swank::*swank-io* t)
           (cl-tron-mcp/swank::*swank-socket* t)
           (cl-tron-mcp/swank::*connection-generation* 4))
      (setf (gethash 31 cl-tron-mcp/swank::*pending-requests*) request)
      (cl-tron-mcp/swank::invalidate-swank-connection
       :expected-io t :expected-socket t :expected-generation 4)
      (ok (cl-tron-mcp/swank::swank-request-completed-p request))
      (ok (string= "SWANK_CONNECTION_CLOSED"
                   (getf (cl-tron-mcp/swank::swank-request-response request)
                         :code)))
      (ok (null cl-tron-mcp/swank::*current-request-id*))
      (ok (getf (cl-tron-mcp/swank::wait-for-response 31 :timeout 30)
                :error)))))

(deftest failed-write-detaches-and-schedules-endpoint-reconnect-test
  (testing "an interrupted packet write recovers the same Swank endpoint"
    (let ((original-protocol-writer
            (symbol-function 'cl-tron-mcp/swank-protocol:write-message))
          (original-start-reconnect
            (symbol-function 'cl-tron-mcp/swank::start-reconnect-worker))
          (scheduled nil)
          (caught nil)
          (cl-tron-mcp/swank::*write-lock*
            (bordeaux-threads:make-lock "test-recovery-write"))
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-recovery-connection"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-recovery-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*input-request-lock*
            (bordeaux-threads:make-lock "test-recovery-input"))
          (cl-tron-mcp/swank::*request-lock*
            (bordeaux-threads:make-lock "test-recovery-request"))
          (cl-tron-mcp/swank::*pending-requests*
            (make-hash-table :test 'eql))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (cl-tron-mcp/swank::*swank-host* "write.example")
          (cl-tron-mcp/swank::*swank-port* 4888)
          (cl-tron-mcp/swank::*connection-generation* 9)
          (cl-tron-mcp/swank::*reconnect-enabled* t))
      (unwind-protect
           (progn
             (setf (symbol-function
                    'cl-tron-mcp/swank-protocol:write-message)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "broken write"))
                   (symbol-function
                   'cl-tron-mcp/swank::start-reconnect-worker)
                   (lambda (&rest arguments)
                     (setf scheduled arguments)
                     (error "scheduler failed")))
             (setf caught
                   (handler-case
                       (cl-tron-mcp/swank::write-message '(:test))
                     (error (e) e)))
             (ok (typep caught 'error))
             (ok (search "broken write" (princ-to-string caught)))
             (ok (not cl-tron-mcp/swank::*swank-connected*))
             (ok (= 10 (getf scheduled :expected-generation)))
             (ok (string= "write.example" (getf scheduled :host)))
             (ok (= 4888 (getf scheduled :port))))
        (setf (symbol-function 'cl-tron-mcp/swank-protocol:write-message)
              original-protocol-writer
              (symbol-function 'cl-tron-mcp/swank::start-reconnect-worker)
              original-start-reconnect)))))

(deftest partial-swank-connect-rolls-back-published-state-test
  (testing "worker startup failure leaves no half-installed connection"
    (let ((original-connect (symbol-function 'usocket:socket-connect))
          (original-stream (symbol-function 'usocket:socket-stream))
          (original-thread-maker
            (symbol-function 'cl-tron-mcp/logging:make-diagnostic-thread))
          (fake-io
            (make-two-way-stream (make-string-input-stream "")
                                 (make-string-output-stream)))
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-partial-connect"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-partial-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*input-request-lock*
            (bordeaux-threads:make-lock "test-partial-input"))
          (cl-tron-mcp/swank::*request-lock*
            (bordeaux-threads:make-lock "test-partial-request"))
          (cl-tron-mcp/swank::*pending-requests*
            (make-hash-table :test 'eql))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*swank-connected* nil)
          (cl-tron-mcp/swank::*swank-socket* nil)
          (cl-tron-mcp/swank::*swank-io* nil)
          (cl-tron-mcp/swank::*connection-generation* 20))
      (unwind-protect
           (progn
             (setf (symbol-function 'usocket:socket-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     :fake-socket)
                   (symbol-function 'usocket:socket-stream)
                   (lambda (socket)
                     (declare (ignore socket))
                     fake-io)
                   (symbol-function
                    'cl-tron-mcp/logging:make-diagnostic-thread)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "thread startup failed")))
             (let ((result
                     (cl-tron-mcp/swank:swank-connect
                      :host "partial.example" :port 4777)))
               (ok (getf result :error))
               (ok (not cl-tron-mcp/swank::*swank-connected*))
               (ok (null cl-tron-mcp/swank::*swank-socket*))
               (ok (null cl-tron-mcp/swank::*swank-io*))
               (ok (null cl-tron-mcp/swank::*swank-host*))
               (ok (null cl-tron-mcp/swank::*swank-reader-thread*))
               (ok (= 22 cl-tron-mcp/swank::*connection-generation*))))
        (setf (symbol-function 'usocket:socket-connect) original-connect
              (symbol-function 'usocket:socket-stream) original-stream
              (symbol-function
               'cl-tron-mcp/logging:make-diagnostic-thread)
              original-thread-maker)
        (ignore-errors (close fake-io :abort t))))))

(deftest partial-swank-connect-retry-adopts-rollback-generation-test
  (testing "automatic reconnect keeps retry ownership after partial startup"
    (let ((original-connect (symbol-function 'usocket:socket-connect))
          (original-stream (symbol-function 'usocket:socket-stream))
          (original-thread-maker
            (symbol-function 'cl-tron-mcp/logging:make-diagnostic-thread))
          (connect-calls 0)
          (second-attempt-count nil)
          (thread-starts 0)
          (streams nil)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-partial-retry-connect"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-partial-retry-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*input-request-lock*
            (bordeaux-threads:make-lock "test-partial-retry-input"))
          (cl-tron-mcp/swank::*request-lock*
            (bordeaux-threads:make-lock "test-partial-retry-request"))
          (cl-tron-mcp/swank::*pending-requests*
            (make-hash-table :test 'eql))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*swank-connected* nil)
          (cl-tron-mcp/swank::*swank-running* nil)
          (cl-tron-mcp/swank::*swank-socket* nil)
          (cl-tron-mcp/swank::*swank-io* nil)
          (cl-tron-mcp/swank::*swank-reader-thread* nil)
          (cl-tron-mcp/swank::*event-processor-thread* nil)
          (cl-tron-mcp/swank::*heartbeat-thread* nil)
          (cl-tron-mcp/swank::*connection-generation* 20)
          (cl-tron-mcp/swank::*reconnect-enabled* t)
          (cl-tron-mcp/swank::*reconnect-delay* 0)
          (cl-tron-mcp/swank::*reconnect-max-attempts* 3)
          (cl-tron-mcp/swank::*reconnect-attempt-count* 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'usocket:socket-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf connect-calls)
                     (when (= connect-calls 2)
                       (setf second-attempt-count
                             cl-tron-mcp/swank::*reconnect-attempt-count*))
                     (list :fake-socket connect-calls))
                   (symbol-function 'usocket:socket-stream)
                   (lambda (socket)
                     (declare (ignore socket))
                     (let ((stream
                             (make-two-way-stream
                              (make-string-input-stream "")
                              (make-string-output-stream))))
                       (push stream streams)
                       stream))
                   (symbol-function
                    'cl-tron-mcp/logging:make-diagnostic-thread)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf thread-starts)
                     (when (= thread-starts 1)
                       (error "thread startup failed"))
                     (list :fake-thread thread-starts)))
             (let ((result
                     (cl-tron-mcp/swank:attempt-reconnect
                      :expected-generation 20)))
               (ok (getf result :success))
               (ok (= 2 connect-calls))
               (ok (= 2 second-attempt-count))
               (ok (= 23 cl-tron-mcp/swank::*connection-generation*))
               (ok (zerop cl-tron-mcp/swank::*reconnect-attempt-count*))))
        (setf (symbol-function 'usocket:socket-connect) original-connect
              (symbol-function 'usocket:socket-stream) original-stream
              (symbol-function
               'cl-tron-mcp/logging:make-diagnostic-thread)
              original-thread-maker)
        (dolist (stream streams)
          (ignore-errors (close stream :abort t)))))))

(deftest swank-ping-reply-keeps-thread-and-tag-test
  (testing "Swank ping throttling receives its routed pong event"
    (let ((original-write
            (symbol-function 'cl-tron-mcp/swank::write-message))
          (message nil)
          (generation nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank::write-message)
                   (lambda (value &key expected-generation)
                     (setf message value
                           generation expected-generation)))
             (cl-tron-mcp/swank::dispatch-incoming-message
              '(:ping 17 42) 6)
             (ok (equal '(:emacs-pong 17 42) message))
             (ok (= 6 generation)))
        (setf (symbol-function 'cl-tron-mcp/swank::write-message)
              original-write)))))

(deftest stale-reader-cannot-repopulate-connection-session-state-test
  (testing "messages from an invalidated generation cannot enter new state"
    (let ((cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-stale-reader-events"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*input-request-lock*
            (bordeaux-threads:make-lock "test-stale-reader-input"))
          (cl-tron-mcp/swank::*request-lock*
            (bordeaux-threads:make-lock "test-stale-reader-request"))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 4 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*pending-input-requests* nil)
          (cl-tron-mcp/swank::*pending-requests*
            (make-hash-table :test 'eql))
          (cl-tron-mcp/swank::*debugger-thread* nil)
          (cl-tron-mcp/swank::*debugger-level* 0)
          (cl-tron-mcp/swank::*connection-generation* 2))
      (cl-tron-mcp/swank::dispatch-incoming-message
       '(:debug 17 1 ("stale debugger") nil nil nil) 1)
      (cl-tron-mcp/swank::dispatch-incoming-message
       '(:read-string 17 3) 1)
      (cl-tron-mcp/swank::dispatch-incoming-message
       '(:write-string "stale output" nil) 1)
      (ok (zerop (length cl-tron-mcp/swank::*event-queue*)))
      (ok (null cl-tron-mcp/swank::*pending-input-requests*))
      (ok (null cl-tron-mcp/swank::*debugger-thread*))
      (ok (zerop cl-tron-mcp/swank::*debugger-level*)))))

(deftest debugger-return-transition-is-idempotent-test
  (testing "a nested debugger return is applied once across reply and event paths"
    (let ((cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-idempotent-debug-return"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*debugger-thread* 17)
          (cl-tron-mcp/swank::*debugger-level* 2))
      (cl-tron-mcp/swank::enqueue-debugger-event "outer" nil nil)
      (cl-tron-mcp/swank::enqueue-debugger-event "inner" nil nil)
      (cl-tron-mcp/swank::note-debugger-return 2)
      (ok (= 1 cl-tron-mcp/swank::*debugger-level*))
      (ok (= 1 (length cl-tron-mcp/swank::*event-queue*)))
      (cl-tron-mcp/swank::note-debugger-return 2)
      (ok (= 1 cl-tron-mcp/swank::*debugger-level*))
      (ok (= 1 (length cl-tron-mcp/swank::*event-queue*))))))

(deftest late-abort-response-does-not-clear-new-debugger-episode-test
  (testing "an old command response cannot clear a newer debugger entry"
    (let ((original-send-request
            (symbol-function 'cl-tron-mcp/swank::send-request))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-debugger-episode"))
          (cl-tron-mcp/swank::*event-condition*
            (bordeaux-threads:make-condition-variable))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (cl-tron-mcp/swank::*debugger-thread* 17)
          (cl-tron-mcp/swank::*debugger-level* 1)
          (cl-tron-mcp/swank::*debugger-episode* 5))
      (cl-tron-mcp/swank::enqueue-debugger-event "old" nil nil)
      ;; The manually prepared old event should belong to episode five.
      (setf cl-tron-mcp/swank::*debugger-episode* 5)
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank::send-request)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (cl-tron-mcp/swank::note-debugger-return 1)
                     (cl-tron-mcp/swank::enqueue-debugger-event
                      "new" nil nil :thread 18 :level 1)
                     '(:result (:abort nil))))
             (ok (getf (cl-tron-mcp/swank:swank-abort-thread) :success))
             (ok (= 18 cl-tron-mcp/swank::*debugger-thread*))
             (ok (= 1 cl-tron-mcp/swank::*debugger-level*))
             (ok (= 6 cl-tron-mcp/swank::*debugger-episode*)))
        (setf (symbol-function 'cl-tron-mcp/swank::send-request)
              original-send-request)))))

(deftest swank-abort-uses-protocol-operation-test
  (testing "abort uses Swank protocol operations that exist"
    (let ((original-write-message
            (symbol-function 'cl-tron-mcp/swank::write-message))
          (original-send-request
            (symbol-function 'cl-tron-mcp/swank::send-request))
          (cl-tron-mcp/swank::*swank-connected* t)
          (cl-tron-mcp/swank::*swank-running* t)
          (cl-tron-mcp/swank::*swank-socket* t)
          (cl-tron-mcp/swank::*swank-io* t)
          (cl-tron-mcp/swank::*connection-lock*
            (bordeaux-threads:make-lock "test-swank-abort-connection"))
          (cl-tron-mcp/swank::*event-mutex*
            (bordeaux-threads:make-lock "test-swank-abort-events"))
          (cl-tron-mcp/swank::*event-queue*
            (make-array 2 :adjustable t :fill-pointer 0))
          (sent-message nil)
          (sent-form nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-tron-mcp/swank::write-message)
                   (lambda (message &key expected-generation)
                     (declare (ignore expected-generation))
                     (setf sent-message message)
                     t))
             (let ((cl-tron-mcp/swank::*debugger-thread* nil))
               (ok (getf (cl-tron-mcp/swank:swank-abort-thread) :success))
               (ok (equal '(:emacs-interrupt t) sent-message)))
             (setf (symbol-function 'cl-tron-mcp/swank::send-request)
                   (lambda (form &rest arguments)
                     (declare (ignore arguments))
                     (setf sent-form form)
                     '(:result (:abort nil))))
             (let ((cl-tron-mcp/swank::*debugger-thread* 17)
                   (cl-tron-mcp/swank::*debugger-level* 1))
               (let ((result (cl-tron-mcp/swank:swank-abort-thread)))
                 (ok (getf result :success))
                 (ok (string= "SLDB-ABORT" (symbol-name (first sent-form))))
                 (ok (null cl-tron-mcp/swank::*debugger-thread*)))))
        (setf (symbol-function 'cl-tron-mcp/swank::write-message)
              original-write-message
              (symbol-function 'cl-tron-mcp/swank::send-request)
              original-send-request)))))

(deftest repl-get-restarts-tool-test
    (testing "repl_get_restarts accepts frame argument without signaling an error"
             (let ((cl-tron-mcp/unified::*repl-connected* t)
                  (cl-tron-mcp/swank::*debugger-thread* 456)
                  (cl-tron-mcp/swank::*debugger-level* 1))
              (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
                (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
              (cl-tron-mcp/swank::enqueue-debugger-event
               "test condition"
               '(("RETRY" "Retry request") ("ABORT" "Abort request"))
               '((0 "(CAR 42)")))
              (let ((result (handler-case
                                (cl-tron-mcp/tools:call-tool "repl_get_restarts" (list :|frame| 0))
                              (error (e) e))))
                (ok (listp result))
                (ok (equal '(("RETRY" "Retry request") ("ABORT" "Abort request"))
                           (getf result :restarts)))
                (ok (= 456 (getf result :thread)))
                (ok (= 1 (getf result :level)))))))

(deftest tool-camelcase-argument-normalization-test
    (testing "camelCase MCP argument names are normalized to the snake_case keywords handlers validate"
             (let ((original (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart)))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart)
                           (lambda (&key restart_index)
                             (list :restart_index restart_index)))
                     (let ((result (handler-case
                                       (cl-tron-mcp/tools:call-tool "repl_invoke_restart" (list :|restartIndex| 2))
                                     (error (e) e))))
                       (ok (listp result))
                       (ok (= 2 (getf result :restart_index)))))
                (setf (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart) original)))))

(deftest swank-invoke-restart-clears-debugger-state-test
    (testing "swank-invoke-restart clears cached debugger state when Swank reports abort"
             (let ((original (symbol-function 'cl-tron-mcp/swank::send-request))
                  (cl-tron-mcp/swank::*debugger-thread* 789)
                  (cl-tron-mcp/swank::*debugger-level* 1))
               (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
                (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
               (cl-tron-mcp/swank::enqueue-debugger-event
                "test condition"
                '(("ABORT" "Abort request"))
                '((0 "(CAR 42)")))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'cl-tron-mcp/swank::send-request)
                           (lambda (&rest args)
                             (declare (ignore args))
                             (list :result (list :abort nil))))
                     (let ((result (cl-tron-mcp/swank:swank-invoke-restart :restart_index 1)))
                       (ok (listp result))
                       (ok (null cl-tron-mcp/swank::*debugger-thread*))
                       (ok (zerop cl-tron-mcp/swank::*debugger-level*))
                       (ok (getf (cl-tron-mcp/swank:swank-get-restarts) :error))))
                (setf (symbol-function 'cl-tron-mcp/swank::send-request) original)))))
