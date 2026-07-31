# swank_abort

**Short Description:** Abort a thread

**Full Description:** Abort the active debugger when it belongs to the selected thread, or send Swank's protocol-level interrupt to that thread. REQUIRES: swank_connect first.

**Parameters:**

- `threadId`: Numeric thread ID from `swank_threads` (required)

**Returns:** Success status after the debugger exits, or interrupt status when
the selected thread was not already in the debugger.

**Example Usage:**

```lisp
(swank_abort :threadId 12)
```

**Notes:** Requires user approval. An interrupt enters the debugger; when that thread is already in the debugger, this invokes Swank's `SLDB-ABORT` operation.
