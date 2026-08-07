# thread_backtrace

**Short Description:** Get a thread backtrace from Tron's local MCP process

**Full Description:** Request a backtrace for a thread in the process running the Tron
MCP server. This does not target a thread or process in the Lisp image connected
through Swank.

**Parameters:**

- `threadId`: Thread ID to get backtrace for (required)

**Returns:** Thread backtrace with function calls

**Example Usage:**

```lisp
(thread_backtrace :threadId "main")
```

**Notes:** Use `thread_list` to get local Tron thread IDs. For remote-target listing,
use `repl_threads`. This tool is omitted from the focused Codex profile.
