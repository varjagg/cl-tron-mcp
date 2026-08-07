;;;; src/tools/registry.lisp

(in-package :cl-tron-mcp/tools)

(defvar *tool-registry* (make-hash-table :test 'equal))

(defparameter *read-only-tools*
  '("breakpoint_list" "debugger_frames" "debugger_restarts"
    "health_check" "inspect_class" "inspect_function" "inspect_object"
    "inspect_package" "list_callees" "profile_report"
    "repl_backtrace" "repl_completions" "repl_describe" "repl_doc"
    "repl_frame_locals" "repl_get_restarts" "repl_help"
    "repl_list_breakpoints" "repl_status" "repl_threads" "runtime_stats"
    "skill_discover" "skill_recommend" "skill_store_installation_guide"
    "swank_autodoc" "swank_backtrace" "swank_completions"
    "swank_debugger_state" "swank_describe" "swank_get_restarts"
    "swank_process_list" "swank_process_status"
    "swank_status" "swank_threads" "system_info" "thread_backtrace"
    "thread_inspect" "thread_list" "trace_list" "whitelist_status"
    "who_binds" "who_calls" "who_references" "who_sets")
  "Tools that only observe state and can be marked read-only to MCP clients.")

(defparameter *destructive-tools*
  '("repl_abort" "swank_abort" "swank_interrupt" "swank_kill")
  "Tools that can terminate a process or interrupt active target computation.")

(defparameter *codex-profile-tools*
  '("code_compile_string" "gc_run" "health_check" "inspect_class"
    "inspect_function" "inspect_object" "inspect_package" "inspect_slot"
    "list_callees" "profile_report" "profile_start" "profile_stop"
    "reload_system" "repl_abort" "repl_backtrace" "repl_compile"
    "repl_completions" "repl_connect" "repl_continue" "repl_describe"
    "repl_disconnect" "repl_doc" "repl_eval" "repl_frame_locals"
    "repl_get_restarts" "repl_help" "repl_inspect" "repl_invoke_restart"
    "repl_list_breakpoints" "repl_next" "repl_out" "repl_remove_breakpoint"
    "repl_set_breakpoint" "repl_status" "repl_step" "repl_threads"
    "repl_toggle_breakpoint" "runtime_stats" "system_info" "trace_function"
    "trace_list" "trace_remove" "who_binds" "who_calls" "who_references"
    "who_sets")
  "Focused remote-target tool set exposed when TRON_TOOL_PROFILE=codex.")

(defun json-boolean (value)
  "Return VALUE in the marker convention expected by json-compat."
  (if value t :false))

(defun tool-read-only-p (name)
  "Return true when NAME is classified as observation-only."
  (member name *read-only-tools* :test #'string=))

(defun tool-destructive-p (name)
  "Return true when NAME can terminate or interrupt target computation."
  (member name *destructive-tools* :test #'string=))

(defun make-tool-annotations (name)
  "Build MCP-standard behavioral annotations for tool NAME."
  (let ((annotations (make-hash-table :test 'equal))
        (read-only (tool-read-only-p name)))
    (setf (gethash :|readOnlyHint| annotations) (json-boolean read-only))
    (setf (gethash :|destructiveHint| annotations)
          (json-boolean (tool-destructive-p name)))
    (setf (gethash :|idempotentHint| annotations) (json-boolean read-only))
    (setf (gethash :|openWorldHint| annotations) :false)
    annotations))

(defun current-tool-profile ()
  "Return the configured tool exposure profile."
  (cl-tron-mcp/config:get-config :tool-profile :all))

(defun tool-enabled-in-profile-p (name)
  "Return true when tool NAME is available in the active profile."
  (case (current-tool-profile)
    (:codex (and (member name *codex-profile-tools* :test #'string=) t))
    (otherwise t)))

(defun codex-approval-mode-p ()
  "Return true when a local stdio Codex client owns mutating-tool approval."
  (and (eq (cl-tron-mcp/config:get-config :approval-mode :server) :codex)
       (member (cl-tron-mcp/config:get-config :transport :stdio)
               '(:stdio :stdio-only))))

(defun normalize-argument-key (key)
  "Convert an incoming MCP argument key to the snake_case keyword expected by handlers."
  (let* ((raw (string key))
         (normalized
           (with-output-to-string (out)
             (loop for i from 0 below (length raw)
                   for ch = (char raw i)
                   for prev = (and (> i 0) (char raw (1- i)))
                   do (cond
                        ((char= ch #\-)
                         (write-char #\_ out))
                        ((and (alpha-char-p ch)
                              (char= ch (char-upcase ch))
                              (char/= ch (char-downcase ch))
                              prev
                              (or (lower-case-p prev)
                                  (digit-char-p prev)))
                         (write-char #\_ out)
                         (write-char ch out))
                        (t
                         (write-char (char-upcase ch) out)))))))
    (intern normalized :keyword)))

(defstruct tool-entry
  name
  descriptor
  handler)

;;; ------------------------------------------------------------------
;;; Schema normalization (cl-tron-mcp#2, "bug #7")
;;;
;;; ~90 tool registrations across src/tools/*.lisp author :input-schema
;;; and :output-schema as flat plists, e.g.
;;;   (list :objectId "string" :maxDepth "integer")
;;; or the bare-type shorthand
;;;   (list :type "object")
;;; Neither shape is valid JSON Schema: MCP requires
;;;   {"type":"object","properties":{...}}
;;; and every key in that authored plist is an UNESCAPED keyword, which
;;; SBCL's standard readtable upcases at read time (before any of this code
;;; runs) -- :objectId is already, irrecoverably, the symbol OBJECTID by the
;;; time register-tool sees it. The functions below normalize whatever shape
;;; a call site authored into a spec-valid JSON Schema hash-table, at this
;;; single chokepoint, without touching any of the ~90 call sites.
;;;
;;; Property-name casing choice: DOWNCASE (e.g. "objectid"), not an attempt
;;; to reconstruct "objectId". Reconstruction is impossible from a symbol
;;; whose original case is already gone. Downcasing is safe because
;;; NORMALIZE-ARGUMENT-KEY (below) upcases every letter of an incoming
;;; wire-argument key before it ever checks for a camelCase word-boundary,
;;; so "OBJECTID" (today's served spelling) and "objectid" (this fix's
;;; spelling) normalize identically -- both to :OBJECTID. Downcasing the
;;; served property name is therefore a pure cosmetic/shape fix: it changes
;;; no argument-binding behavior relative to the pre-fix baseline, so
;;; NORMALIZE-ARGUMENT-KEY needed no change. (Whether that baseline
;;; argument-binding is itself fully correct for every camelCase-vs-
;;; snake_case handler parameter is a separate, deeper, pre-existing
;;; question this fix does not touch -- see task-14 report.)

(defparameter *json-schema-primitive-types*
  '("object" "array" "string" "number" "integer" "boolean" "null")
  "The JSON Schema §6.1.1 \"type\" keyword's valid primitive values.
Claude Code's MCP client validates every property's \"type\" against
exactly this set and refuses the whole tools/list response (\"Connected
tools fetch failed\", reason \"type must be JSONType or JSONType[]\") if
any single one doesn't match -- confirmed via `claude mcp get cl-tron`
against src/installation/installation.lisp's skill_discover tool, authored
with :skill_registry \"map\" (a non-standard, made-up type name).")

(defun canonical-json-schema-type (type-name)
  "Coerce an authored type-name string to a valid JSON Schema primitive
type. Falls back to \"object\" for anything not in
*JSON-SCHEMA-PRIMITIVE-TYPES* (case-insensitively) -- e.g. \"map\", used at
one call site to mean a key/value dictionary, which IS a JSON object."
  (if (member type-name *json-schema-primitive-types* :test #'string-equal)
      (string-downcase type-name)
      "object"))

(defun schema-keyword-plist-p (value)
  "T if VALUE looks like a nested schema-keyword plist as authored at a
tool-registration call site, e.g. (:enum (\"a\" \"b\")): a non-empty list of
even length whose even-position elements are all keywords."
  (and (consp value)
       (evenp (length value))
       (loop for (k nil) on value by #'cddr always (keywordp k))))

(defun schema-keyword-plist->hash (plist)
  "Convert a nested schema-keyword plist (keys are JSON Schema keywords such
as :enum or :description -- single lowercase words, so downcasing recovers
their exact spelling losslessly) into a hash-table with lowercase string
keys. A value that is itself a schema-keyword plist recurses; anything else
(e.g. an :enum's list of allowed literal values) passes through unchanged so
it serializes as a plain JSON array/scalar, not a nested schema."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (string k)) h)
                   (if (schema-keyword-plist-p v)
                       (schema-keyword-plist->hash v)
                       v)))
    h))

(defun schema-property-fragment (value)
  "Convert one authored property VALUE into a JSON Schema fragment:
a bare type-name string (\"string\", \"integer\", ...) becomes {\"type\":
value}; a nested schema-keyword plist (e.g. (:enum (...))) recurses via
SCHEMA-KEYWORD-PLIST->HASH; anything else passes through unchanged."
  (cond
    ((stringp value)
     (let ((h (make-hash-table :test 'equal)))
       (setf (gethash "type" h) (canonical-json-schema-type value))
       h))
    ((schema-keyword-plist-p value)
     (schema-keyword-plist->hash value))
    (t value)))

(defun normalize-tool-schema (schema)
  "Convert an authored :input-schema/:output-schema plist into a spec-valid
JSON Schema hash-table: {\"type\":\"object\",\"properties\":{...}}.

SCHEMA is one of:
  NIL                                     -- a no-argument tool
  (:type \"sometype\")                     -- bare-type shorthand, used
                                             throughout as a schema-less
                                             \"result is a JSON object\"
                                             output annotation (never used
                                             for a real, multi-property
                                             schema, so a single (:type ...)
                                             pair is unambiguous)
  (:propName \"type\" :propName2 ... )     -- flat property-name -> type
                                             (or nested descriptor) map

NIL always becomes {\"type\":\"object\",\"properties\":{}} -- an empty
HASH-TABLE, not an empty list, because json-compat's encode-tree serializes
NIL as \"[]\"; \"properties\" must serialize as \"{}\" when empty."
  (let ((h (make-hash-table :test 'equal)))
    (cond
      ((null schema)
       (setf (gethash "type" h) "object")
       (setf (gethash "properties" h) (make-hash-table :test 'equal)))
      ((and (= (length schema) 2)
            (keywordp (first schema))
            (string-equal (string (first schema)) "type")
            (stringp (second schema)))
       (setf (gethash "type" h) (canonical-json-schema-type (second schema))))
      (t
       (setf (gethash "type" h) "object")
       (let ((props (make-hash-table :test 'equal)))
         (loop for (k v) on schema by #'cddr
               do (setf (gethash (string-downcase (string k)) props)
                        (schema-property-fragment v)))
         (setf (gethash "properties" h) props))))
    h))

(defun register-tool (name description &key input-schema output-schema requires-approval documentation-uri concurrency)
  "Register a tool with descriptor and handler.
   requires-approval t means approvalLevel \"user\" (human must approve); nil means \"none\" (auto-run)."
  (let ((descriptor (make-hash-table :test 'equal)))
    ;; NOTE: keys are pipe-escaped (:|name|, not :name) so the reader preserves
    ;; their lowercase/camelCase spelling. An unescaped :name reads as the
    ;; symbol NAME (all upper), which json-compat's encode-tree serializes via
    ;; (string key) -- producing "NAME" in the wire JSON instead of "name".
    ;; This is the same convention handlers.lisp/handlers-initialize.lisp
    ;; already use for outgoing JSON-RPC keys; see cl-tron-mcp#2.
    (setf (gethash :|name| descriptor) name)
    (setf (gethash :|description| descriptor) description)
    ;; inputSchema/outputSchema: normalized to spec-valid JSON Schema shape
    ;; (see NORMALIZE-TOOL-SCHEMA above) -- this is "bug #7", the layer below
    ;; the top-level descriptor-key fix already applied to this file.
    (setf (gethash :|inputSchema| descriptor) (normalize-tool-schema input-schema))
    (setf (gethash :|outputSchema| descriptor) (normalize-tool-schema output-schema))
    (setf (gethash :|annotations| descriptor) (make-tool-annotations name))
    (setf (gethash :|requiresApproval| descriptor) (json-boolean requires-approval))
    (setf (gethash :|approvalLevel| descriptor) (if requires-approval "user" "none"))
    (setf (gethash :|documentationUri| descriptor) documentation-uri)
    (setf (gethash :|concurrency| descriptor) (or concurrency "sequential"))
    (setf (gethash name *tool-registry*)
          (make-tool-entry :name name
                           :descriptor descriptor
                           :handler nil))))

(defun register-tool-handler (name handler)
  "Register handler function for already registered tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (setf (tool-entry-handler entry) handler))))

(defun list-tool-descriptors ()
  "Get tool descriptors enabled by the active tool profile."
  (let ((descriptors (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (name entry)
               (when (tool-enabled-in-profile-p name)
                 (vector-push-extend (tool-entry-descriptor entry) descriptors)))
             *tool-registry*)
    (coerce descriptors 'list)))

(defun get-tool-handler (name)
  "Get handler function for tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-handler entry))))

(defun get-tool-descriptor (name)
  "Get tool descriptor (hash table) for tool NAME, or nil if unknown."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-descriptor entry))))

(defun tool-requires-user-approval-p (name)
  "Return t if tool NAME has approval level user (requires human approval)."
  (let ((desc (get-tool-descriptor name)))
    (and desc (eq (gethash :|requiresApproval| desc) t))))

(defun call-tool (name arguments)
  "Call tool by name with arguments plist.
The plist keys are JSON-style (e.g., :|port|) and converted to proper keywords (:PORT)."
  (let ((handler (get-tool-handler name)))
    (unless (tool-enabled-in-profile-p name)
      (error "Tool disabled by ~a profile: ~a" (current-tool-profile) name))
    (unless handler
      (error "Unknown tool: ~a" name))
    (let ((args-list (loop for (key value) on arguments by #'cddr
                           for keyword = (normalize-argument-key key)
                           append (list keyword value))))
      (apply handler args-list))))

(defun load-all-tools ()
  "No-op: tool files are loaded by ASDF in dependency order as declared in cl-tron-mcp.asd.
This function is kept for backward compatibility but performs no action."
  nil)
