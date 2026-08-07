# thread_inspect

**Short Description:** Inspect a thread in Tron's local MCP process

**Full Description:** Get information about a thread in the process running the Tron
MCP server. This does not inspect a thread or process in the Lisp image connected
through Swank.

**Parameters:**

- `threadId`: Thread ID to inspect (required)

**Returns:** Thread information including name, state, and stack usage

**Example Usage:**

```lisp
(thread_inspect :threadId "main")
```

**Notes:** Use `thread_list` to get local Tron thread IDs. For remote-target listing,
use `repl_threads`. This tool is omitted from the focused Codex profile.
