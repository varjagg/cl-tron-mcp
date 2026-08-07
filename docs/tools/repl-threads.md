# repl_threads

**Short Description:** List threads/processes in the connected remote Lisp image

**Full Description:** Query the connected remote Lisp image through Swank. REQUIRES:
`repl_connect` first. Shows target thread or process names, status, and IDs for
debugging concurrency issues. This is the listing tool to use for the application
being debugged.

**Parameters:** None

**Returns:** List of thread objects with name, status, and ID

**Example Usage:**

```lisp
(repl_threads)
```

**Notes:** Swank may hide the transient worker executing the listing request. Do not
substitute `thread_list`, which examines Tron's local MCP process, or
`swank_process_list`, which only lists child images launched through `swank_launch`.
