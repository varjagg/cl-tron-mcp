# INTERRUPT_ERROR

**Error Code:** `INTERRUPT_ERROR`

**Message:** Interrupt error: {error_message}

## Description

This error occurs when an interrupt operation fails. The specific error message provides more details about what went wrong.

## Common Causes

- No active connection to interrupt
- Thread not found or already terminated
- Permission issues
- Server not responding to interrupt

## Resolution

1. Check the specific error message for details
2. Verify connection is active
3. Check thread status
4. Ensure server is running and responsive

## Example

```lisp
;; Check connection status
(swank_status)

;; List threads/processes in the connected target
(repl_threads)

;; Retry interrupt with correct parameters
(swank_interrupt :thread-id "thread-123")
```

## Related Tools

- `swank_interrupt` - Interrupt execution
- `repl_threads` - List threads/processes in the connected target
- `swank_status` - Check connection status
