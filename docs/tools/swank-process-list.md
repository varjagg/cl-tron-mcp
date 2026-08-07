# swank_process_list

**Short Description:** List Tron-managed SBCL+Swank processes

**Full Description:** Return the current contents of Tron's managed-process registry,
including port, PID, host, uptime, and communication style for each child image
launched through `swank_launch`.

**Parameters:** None

**Returns:** A success plist containing `count` and `processes`

**Example Usage:**

```lisp
(swank_process_list)
```

**Notes:** This does not query the connected remote REPL and is not an OS process
listing. Use `repl_threads` for threads or processes in the connected target.
