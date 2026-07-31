# swank_eval

**Short Description:** Evaluate code in SBCL

**Full Description:** Evaluate Lisp code in the connected SBCL session. REQUIRES: swank_connect first. Code runs in a persistent session - state is preserved across calls. Use for testing, debugging, and hot-patching code.

**Parameters:**

- `code`: Lisp code to evaluate (required)
- `package`: Package to evaluate in (optional, default: CL-USER)
- `timeout`: Maximum time Tron waits for a Swank response, in seconds (optional, default: 300, maximum: 3600)

**Returns:** Evaluation result with value, output, and any errors

**Example Usage:**

```lisp
(swank_eval :code "(+ 1 2 3)")
(swank_eval :code "(defun foo (x) (* x 2))" :package "CL-USER")
(swank_eval :code "(long-running-job)" :timeout 900)
```

**Notes:** Requires user approval. State is preserved across calls - variables and functions defined in one call are available in subsequent calls. A wait timeout does not terminate code already running in the target image; use `swank_interrupt` or `repl_abort` to interrupt it.
