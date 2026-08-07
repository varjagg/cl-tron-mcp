# Local Tron Thread Diagnostics

The `thread_*` tools inspect the process running the Tron MCP server. They do not
cross the Swank connection and therefore do not describe the application being
debugged.

For the connected remote Lisp image, use `repl_threads`. On implementations such as
Clozure CL, Swank calls these runtime objects processes; `repl_threads` is still the
correct tool.

## Tools Overview

| Tool | Scope | Purpose | Approval Required |
| --- | --- | --- | --- |
| `thread_list` | Local Tron process | List local MCP threads | No |
| `thread_inspect` | Local Tron process | Inspect local MCP thread state | No |
| `thread_backtrace` | Local Tron process | Request a local MCP thread backtrace | No |
| `repl_threads` | Connected remote target | List target threads/processes via Swank | No |

The focused Codex profile exposes `repl_threads` and omits the local-only `thread_*`
tools. The full profile retains them for diagnosing Tron itself.

## Managed Processes Are Different

`swank_process_list` reads Tron's managed-child registry. It only includes Lisp images
created through `swank_launch`; it is neither a remote-target thread listing nor an OS
process listing.

## See Also

- [repl_threads](repl-threads.md)
- [thread_list](thread-list.md)
- [swank_process_list](swank-process-list.md)
- [Debugging workflows](../../prompts/debugging-workflows.md)
