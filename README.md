# strace-viewer

Using [strace](https://man7.org/linux/man-pages/man1/strace.1.html) to trace
the system calls made by a process is a useful way to gain insight into its
behaviour and is often invaluable when debugging.  When tracing a single
process the log output is easy enough to understand in its raw form.  However
the raw form quickly becomes difficult to make sense of when `strace` is made
to follow child processes (`-f` option) and log the syscalls for each process
to a separate file (`-ff -o` options).  If lots of processes are spawned there
might be hundreds of megabytes of logs across numerous files.

This project is intended to make it easier to understand and explore the logs
when `strace` is run in the latter mode. It has two components:
- **strace-import**: uses
  [inotify](https://man7.org/linux/man-pages/man7/inotify.7.html) for watch for
changes to strace log files and import them into a
[sqlite](https://www.sqlite.org/index.html) database.

  ![strace-import](/strace-import.png)

- **log-view**: serves a JavaScript application for viewing and exploring the
  imported logs graphically.

  ![log-view](/log-view.png)

The log-view component is unfinished and still requires quite a lot of work,
but [the strace-import
component](https://github.com/benradf/strace-viewer/blob/ef1376802b4d76450a5688b51d88ea92d56ba25b/code/log-view/Strace.hs#L562-L634)
is fully functional.  Even without the viewer it can be useful to import strace
logs into a sqlite database so they can be manually queried for insights.
