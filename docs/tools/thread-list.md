# thread_list

**Short Description:** List threads in Tron's local MCP process

**Full Description:** List threads in the process running the Tron MCP server, with
their status (running, waiting, etc.). This does not query the Lisp image connected
through Swank.

**Parameters:** None

**Returns:** List of threads with status information

**Example Usage:**

```lisp
(thread_list)
```

**Notes:** Use `repl_threads` for the connected remote application. This local tool is
for debugging Tron itself and is omitted from the focused Codex tool profile.
