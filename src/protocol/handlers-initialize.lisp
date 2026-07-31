;;;; src/protocol/handlers-initialize.lisp
;;;;
;;;; Initialize handler for JSON-RPC protocol.
;;;;
;;;; This file contains:
;;;;   - initialize handler (server handshake and capability negotiation)

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Initialize Handler
;;; ============================================================

(defun server-instructions ()
  "Return concise cross-tool guidance for MCP clients such as Codex."
  (format nil (concatenate
               'string
               "Tron debugs a long-running SBCL through Swank. First call repl_status; "
               "if disconnected call repl_connect for 127.0.0.1:~d. Prefer unified "
               "repl_* tools over raw swank_* tools. Inspect before modifying, preserve "
               "the target session, and never restart or kill it unless the user "
               "explicitly asks. Codex performs approval for mutating tools when "
               "TRON_APPROVAL_MODE=codex.")
          (cl-tron-mcp/config:get-config :swank-port 4006)))

(defun handle-initialize (id params)
  "Handle initialize request.
Returns server capabilities including tools, resources, and prompts.
This is the first message sent by MCP clients during handshake.
Response is used by both stdio and HTTP transports unchanged."
  (declare (ignore params))
  (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|result| (list :|protocolVersion| "2024-11-05"
                                          :|capabilities| (list
                                                           ;; Tools capability - allows model-controlled tool invocation
                                                           :|tools| (list :|listChanged| t)
                                                           ;; Resources capability - exposes documentation
                                                           :|resources| (list :|subscribe| :false
                                                                              :|listChanged| t)
                                                           ;; Prompts capability - exposes guided workflows
                                                          :|prompts| (list :|listChanged| t))
                                          :|serverInfo| (list :|name| "cl-tron-mcp"
                                                              :|version| cl-tron-mcp/config:*version*)
                                          :|instructions| (server-instructions)))))

(provide :cl-tron-mcp/protocol-handlers-initialize)
