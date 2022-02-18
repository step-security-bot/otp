# 2022 Erlang 🐚 plan

This is the work document for a rewrite of the Erlang shell and terminal handling.

The goal of the rewrite is to provide a better shell and escript experience for the users of Erlang.

Specific goals are:

- [ ] Make the shell experience on Windows and unix equivalent (MUST)
- [ ] Allow `erl` and `escript` to be used in unix pipes (MUST)
- [ ] Fix unicode detection to work properly for both tty and non-tty shells (MUST)
- [ ] Fix bugs where console after exit is in an incorrect state (MUST)
- [ ] Allow multiline editing of the current statement (SHOULD)
- [ ] The shell history should be work in statements, not lines (SHOULD)
- [ ] -remsh work without tty (SHOULD)
- [ ] Allow definition of functions in shell (SHOULD)
- [ ] Allow definition of modules in shell (COULD)
- [ ] Better autocomplete support (COULD)
- [ ] Better support for ansi escape sequences (SHOULD)
- [ ] If a shell command is blocked for more than X seconds in a receive we ask the user if they want to interrupt the command (COULD)
- [ ] dbg should redirect I/O to remote shells (COULD)

Tickets that should be solved:
- [ ] [ERL-1069: Support disabling echo input in new shell](https://github.com/erlang/otp/issues/4337)
- [ ] [ERL-891: Remote Shells can get stuck in compute extensive busy loop](https://github.com/erlang/otp/issues/4343)
- [ ] [ERL-1156: Shell command fail on nodes started from escript](https://github.com/erlang/otp/issues/4137)
- [ ] [ERL-424: Unicode Characters Not Displayed Correctly In Windows Console](https://github.com/erlang/otp/issues/3390)
- [ ] [ERL-331: Erlang shell should handle EOF signal (Ctrl+D)](https://github.com/erlang/otp/issues/4414)
- [ ] [ERL-282: escript fails when redirecting stdout to less and less terminates](https://github.com/erlang/otp/issues/3150)
- [ ] [ERL-162: "erl -remsh" malfunctions when TERM is set to "dumb"](https://github.com/erlang/otp/issues/4220)

# 2022 🐚 idea

In order to be able to use dirty schedulers for doing read and write to the console we should move the ttysl driver code to a nif. We want to use the dirty schedulers because NON_BLOCKING I/O does not work.

We should strive to never use the "oldshell" unless asked by the user. Instead in places where isatty returns false we should use the tty nifs without any tty support. The reason for this is because we want as much of the code to be as similar as possible, so that unicode detection and -remsh work no matter if we have a tty or not.

## Better windows support

MS have recently added support for [Console Virtual Terminal Sequences](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences).

This allows the implementation of unix and windows to be almost identical. It would also allow us to remove all werl code.

# Rough overview of how things work in OTP 24

There are two important different modes that operate, "newshell" and "oldshell".

These modes work very differently and how data is routed in the system varies depending on which one is used.

There is a great description here: https://ferd.ca/repl-a-bit-more-and-less-than-that.html

## New Shell

This mode is used when either:
* `erl` is started on unix where a tty capable of input/output
* `werl` is started on windows

When in this mode there are three processes and one port involved in I/O.

1. The `user` process  
  The user process receives all I/O requests from any application or any code that does an `io:format(user,"hello",[]).`. In this mode the `user` process is implemented in `group.erl` without a shell attached.  
2. The `user_drv` process  
  The `user_drv` process is the process talking to the ttsl driver. It receives all output requests from Erlang code and sends all input to the current `shell` process. This process is also responsible for handling ^G and all the interaction that happens when that mode is enabled such as switching in between the active shells or creating new shells.
3. The `shell` process  
  One such process is created per active shell. The `shell` process is implemented in `group.erl` with an active shell and the actual shell logic is in `edlin.erl`. Any output done in a shell process is sent to `user_drv` directly without passing through `user`.
4. The `ttsl` port  
  The ttsl driver basically just outputs UTF-8 characters to stdout. It is also responsible for dealing with any terminal inconsistencies and keeping track of the cursor in the terminal window. On windows this diver talks to the werl program to move things about there.

Some ideas for improvements here:
1. Do we really need the current `user` process? Could it be `user_drv` directly?
2. Can we refactor out the ^G handling so that it works as a specialized shell instead of having the logic in `user_drv`?

## Old shell

This mode is used when either:
1. `escript` is started
2. `erl.exe`
3. `erl` when input or output are not ttys
3. `erl` when we do not have termcap, or `-noshell` or `-oldshell` is configured.

When in this mode there is one process and one port involved in I/O.

1. The `user` process  
  In oldshell mode there is only one process that takes care of everything and that is the `user` process. All of the logic for handling the code is in `user.erl`. The user process starts a shell if the `-noshell` flag is not given.
2. The fd driver for 0/1  
  The is just responsible for getting bytes in and out. It does some crazy heuristic about whether it can set the fds in NON_BLOCKING mode or and and it it cannot is used the async thread pool to output data. 