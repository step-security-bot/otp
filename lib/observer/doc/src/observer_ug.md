# Observer

## Introduction

Observer is a graphical tool for observing the characteristics of Erlang systems. Observer displays system information, application supervisor trees, process information, ETS tables, Mnesia tables and contains a front end for Erlang tracing.

## Getting Started

Run Observer from a standalone node to minimize the impact of the system being observed.

*Example:*

```text
% erl -sname observer -hidden -setcookie MyCookie -run observer
```

Select the node to observe with menu *Nodes*. Menu *View > Refresh interval* controls how often the view is to be updated. The refresh interval is set per viewer so you can have different settings for each viewer. To minimize the system impact, only the active viewer is updated. Other views are updated when activated.

The mouse buttons behave as expected. Use left-click to select objects, right-click to get a menu with the most used options, and double-click to display information about the selected object. In most viewers with many columns, you can change the sort order by left-clicking the column header.

## System Tab

Tab *System* displays general information about the active Erlang node and its runtime system, such as build configuration, system capabilities, and overall use statistics.

## Load Charts Tab

Tab *Load Charts* displays graphs of the current resource use on the active Erlang node.

Graph `Scheduler Utilization` shows scheduler use per scheduler, where each scheduler use has a unique color.

Graph `Memory Usage` shows the total memory use and per memory category use, where each category has a unique color. The categories are as follows:

* __`Total`__ - The sum of all memory categories.

* __`Processes`__ - The sum of all process memory used.

* __`Atom`__ - The size used by the atom table.

* __`Binary`__ - The sum of all off-heap binaries allocated.

* __`Code`__ - The memory allocated for code storage.

* __`Ets`__ - The used memory for all ETS tables.

Graph `IO Usage` shows the current I/O load on the system.

## Memory Allocators Tab

Tab *Memory Allocators* displays detailed information of the carrier size and current memory carriers. For details about memory carriers, see module [`erts_alloc`](`p:erts:erts_alloc.md`) in application ERTS.

The `Max Carrier size` column shows the maximum value seen by observer since the last node change or since the start of the application, i.e. switching nodes will reset the max column. Values are sampled so higher values may have existed than what is shown.

## Applications Tab

Tab *Applications* presents application information. Select an application in the left list to display its supervisor tree. The right-click options in the tree are as follows:

* __Process info__ - Opens a detailed information window on the selected process, including the following:

  * __Process Information__ - Shows the process information.

  * __Messages__ - Shows the process messages.

  * __Dictionary__ - Shows the process dictionary.

  * __Stack Trace__ - Shows the process current stack trace.

  * __State__ - Shows the process state.

  * __Log__ - If enabled and available, shows the process SASL log entries.

  

* __Trace process__ - Adds the selected process identifier to tab *Trace Overview* plus the node that the process resides on.

* __Trace named process__ - Adds the registered name of the process. This can be useful when tracing on many nodes, as processes with that name are then traced on all traced nodes.

* __Trace process tree__ - Adds the selected process and all processes below, right of it, to tab *Trace Overview*.

* __Trace named process tree__ - Adds the selected process and all processes below, right of it, to tab *Trace Overview*.

## Processes Tab

Tab *Processes* lists process information in columns. For each process the following information is displayed:

* __Pid__ - The process identifier.

* __Reds__ - The number of reductions executed on the process. This can be presented as accumulated values or as values since the last update.

* __Memory__ - The size of the process, in bytes, obtained by a call to `process_info(Pid,memory)`.

* __MsgQ__ - The length of the message queue for the process.

Option *Process info* opens a detailed information window on the process under the mouse pointer, including the following:

* __Process Information__ - Shows the process information.

* __Messages__ - Shows the process messages.

* __Dictionary__ - Shows the process dictionary.

* __Stack Trace__ - Shows the process current stack trace.

* __State__ - Shows the process state.

* __Log__ - If enabled and available, shows the process SASL log entries.

> #### Note {: class=info }
> *Log* requires application SASL to be started on the observed node, with `log_mf_h` as log handler. The Observed node must be Erlang/OTP R16B02 or higher. The `rb` server must not be started on the observed node when clicking menu *Log > Toggle log view*. The `rb` server is stopped on the observed node when exiting or changing the observed node.

Option *Trace selected processes* adds the selected process identifiers to tab *Trace Overview* plus the node that the processes reside on.

Option *Trace selected processes by name* adds the registered name of the processes. This can be useful when tracing is done on many nodes, as processes with that name are then traced on all traced nodes.

Option *Kill process* brutally kills the processes under the mouse pointer by sending an exit signal with reason `kill`.

## Ports Tab

Tab *Ports* lists port information in columns. For each port the following information is displayed:

* __Id__ - The port identifier.

* __Connected__ - The process identifier for the process that owns the port.

* __Name__ - The registered name of the port, if any.

* __Controls__ - The name of the command set by `erlang:open_port/2`.

* __Slot__ - The internal index of the port.

Option *Port info* opens a detailed information window for the port under the mouse pointer. In addition to the information above, it also shows links and monitors.

Option *Trace selected ports* adds the selected port identifiers, and the nodes that the ports reside on, to tab *Trace Overview*.

Option *Trace selected ports by name* adds the registered name of the port to tab *Trace Overview*. This can be useful when tracing is done on many nodes, as ports with that name are then traced on all traced nodes.

Option *Close* executes `erlang:port_close/1` on the port under the mouse pointer.

## Sockets Tab

Tab *Sockets* is divided into two parts. The first part contains general `m:socket` information and the second part lists socket information in columns.

For each socket the following information is displayed:

* __Id__ - The socket identifier.

* __Owner__ - The process identifier for the process that owns the socket.

* __Fd__ - The underlying file descriptor of the socket.

* __Domain__ - The communication domain (e.g. inet or inet6) of this socket.

* __Type__ - The type (e.g. stream or dgram) of this socket.

* __Protocol__ - The protocol (e.g. tcp or udp) of this socket.

* __Read State__ - The read state of the socket.

* __Write State__ - The write state of the socket.

Option *Socket info* opens a detailed information window for the socket under the mouse pointer. In addition to the information above, it also shows monitors.

Option *Close* executes `socket:close/1` on the socket under the mouse pointer.

## Table Viewer Tab

Tab *Table Viewer* lists tables. By default, ETS tables are displayed whereas unreadable private ETS tables and tables created by OTP applications are not displayed. Use menu *View* to view "system" ETS tables, unreadable ETS tables, or Mnesia tables.

Double-click to view the table content, or right-click and select option *Show Table Content*. To view table information, select the table and activate menu *View > Table information*, or right-click and select option *Table info*.

You can use [regular expressions](`m:re`) and search for objects, and edit or delete them.

## Trace Overview Tab

Tab *Trace Overview* handles tracing. Trace by selecting the processes or ports to be traced and how to trace them. For processes, you can trace messages, function calls, scheduling, garbage collections, and process-related events such as `spawn`, `exit`, and many others. For ports, you can trace messages, scheduling and port-related events.

To trace function calls, you also need to set up *trace patterns*. Trace patterns select the function calls to be traced. The number of traced function calls can be further reduced with *match specifications*. Match specifications can also be used to trigger more information in the trace messages.

You can also set match specifications on messages. By default, if tracing messages, all messages sent and/or received by the process or port are traced. Match specifications can be used to reduce the number of traced messages and/or to trigger more information in the trace messages.

> #### Note {: class=info }
> Trace patterns only apply to the traced processes and ports.

Processes are added from the *Applications* or *Processes* tabs. Ports are added from the *Ports* tab. A special *new* identifier, meaning all processes, or ports, started after trace start, can be added with buttons *Add 'new' Processes* and *Add 'new' Ports*, respectively.

When adding processes or ports, a window with trace options is displayed. The chosen options are set for the selected processes/ports. To change the options, right-click the process or port and select *Edit process options*. To remove a process or port from the list, right-click and select *Remove process* or *Remove port*, respectively.

Processes and ports added by process/port identifiers add the nodes these processes/ports reside on in the node list. More nodes can be added by clicking button *Add Nodes*, or by right-clicking in the *Nodes* list and select *Add Nodes*. To remove nodes, select them, then right-click and choose *Remove nodes*.

If function calls are traced, trace patterns must be added by clicking button *Add Trace Pattern*. Select a module, function(s), and a match specification. If no functions are selected, all functions in the module are traced.

Trace patterns can also be added for traced messages. Click button *Add Trace Pattern* and select *Messages sent* or *Messages received*, and a match specification.

A few basic match specifications are provided in the tool, and you can provide your own match specifications. The syntax of match specifications is described in the [`ERTS User's Guide`](`p:erts:match_spec.md`). To simplify the writing of a match specification, they can also be written as `fun/1`. For details, see module `m:ms_transform` in application STDLIB.

Click button *Start Trace* to start the trace. By default, trace output is written to a new window. Tracing is stopped when the window is closed, or when clicking button *Stop Trace*. Trace output can be changed with menu *Options > Output*. The trace settings, including match specifications, can be saved to, or loaded from, a file.

For details about tracing, see module `m:dbg` in application Runtime_Tools and in section "Match specifications in Erlang" in [`ERTS User's Guide`](`p:erts:match_spec.md`) and in module `m:ms_transform` in application STDLIB.
