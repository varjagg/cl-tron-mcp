;;;; src/tools/thread-tools.lisp
;;;; Thread tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "thread_list"
  "List threads in Tron's local MCP process, not the connected remote REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/thread-list.md"
  :function cl-tron-mcp/sbcl:list-threads)

(define-validated-tool "thread_inspect"
  "Inspect a thread in Tron's local MCP process, not the connected remote REPL"
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/thread-inspect.md"
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/sbcl:inspect-thread :thread-id thread_id))

(define-validated-tool "thread_backtrace"
  "Get a backtrace for a thread in Tron's local MCP process, not the remote REPL"
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/thread-backtrace.md"
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/sbcl:thread-backtrace :thread-id thread_id))
