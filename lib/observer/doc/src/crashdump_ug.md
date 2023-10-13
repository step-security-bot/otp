# Crashdump Viewer

## Introduction

The Crashdump Viewer is a WxWidgets based tool for browsing Erlang crashdumps.

## Getting Started

The easiest way to start Crashdump Viewer is to use shell script `cdv` with the full path to the Erlang crashdump as argument. The script is located in directory `priv` of the Observer application. This starts the Crashdump Viewer GUI and loads the specified file. If no filename is specified, a file dialog is opened where the file can be selected.

Under Windows, the batch file `cdv.bat` can be used.

Crashdump Viewer can also be started from an Erlang node by calling `crashdump_viewer:start/0` or `crashdump_viewer:start/1`.

## GUI

The GUI main window is opened when Crashdump Viewer has loaded a crashdump. It contains a title bar, a menu bar, information tabs, and a status bar.

The title bar shows the name of the currently loaded crashdump.

The menu bar contains a *File* menu and a *Help* menu. From the *File* menu, a new crashdump can be loaded or the tool can be terminated. From the *Help* menu, this User's Guide and section "How to interpret the Erlang crash dumps" from the ERTS application can be opened. "How to interpret the Erlang crash dumps" describes the raw crashdumps in detail and includes information about each field in the information pages."How to interpret the Erlang crash dumps" is also available in the OTP online documentation.

The status bar at the bottom of the window shows a warning if the currently loaded dump is truncated.

The center area of the main window contains the information tabs. Each tab displays information about a specific item or a list of items. Select a tab by clicking the tab title.

From tabs displaying lists of items, for example, the *Processes* tab or the *Ports* tab, a new window with more information can be opened by double-clicking a row or by right- clicking the row and selecting an item from the drop-down menu. The new window is called a detail window. Detail windows can be opened for processes, ports, nodes, and modules.

The information shown in a detail window can contain links to processes or ports. Clicking one of these links opens the detail window for the process or port in question. If the process or port resides on a remote node, no information is available. Clicking the link then displays a dialog where you can choose to open the detail window for the remote node.

Some tabs contain a left-hand menu where subitems of the information area can be selected. Click one of the rows, and the information is displayed in the right-hand information area.

## Tab Content

Each tab in the main window contains an information page. If no information is found for an item, the page is empty. The reason for not finding information about an item can be the following:

* It is a dump from an old OTP release in which this item was not written.
* The item was not present in the system at the point of failure.
* The dump is truncated. In this case, a warning is displayed in the status bar of the main window.



Even if some information about an item exists, there can be empty fields if the dump originates from an old OTP release.

The value `-1` in any field means "unknown", and in most cases it means that the dump was truncated somewhere around this field.

The following sections describe some of the fields in the information tabs. These are fields that do not exist in the raw crashdump, or in some way differ from the fields in the raw crashdump. For details about other fields, see the [ERTS User's Guide](`p:erts:index.html`), section "How to interpret the Erlang crash dumps". That section can also be opened from the *Help* menu in the main window. There are also links from the following sections to related information in "How to interpret the Erlang crash dumps".

[](){: id=general_info }
## General Tab

Tab *General* shows a short overview of the dump.

The following fields are not described in the ERTS User's Guide:

* __`Crashdump created on`__ - Time of failure.

* __`Memory allocated`__ - The total number of bytes allocated, equivalent to `c:memory(total)`.

* __`Memory maximum`__ - The maximum number of bytes that has been allocated during the lifetime of the originating node. This is only shown if the Erlang runtime system is run instrumented.

* __`Atoms`__ - If available in the dump, this is the total number of atoms in the atom table. If the size of the atom table is unavailable, the number of atoms visible in the dump is displayed.

* __`Processes`__ - The number of processes visible in the dump.

* __`ETS tables`__ - The number of ETS tables visible in the dump.

* __`Funs`__ - The number of funs visible in the dump.

For details, see [General Information](`p:erts:crash_dump.md#general_info`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=processes }
## Processes Tab

Tab *Processes* shows a list of all processes found in the crashdump, including brief information about each process. By default, the processes are sorted by their pids. To sort by another topic, click the desired column heading.

Column *Memory* shows the 'Memory' field that was added to crashdumps in Erlang/OTP R16B01. This is the total amount of memory used by the process. For crashdumps from earlier releases, this column shows the 'Stack+heap' field. The value is always in bytes.

To view detailed information about a specific process, double- click the row in the list, or right-click the row and select *Properties for <pid>*.

For details, see [Process Information](`p:erts:crash_dump.md#processes`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=ports }
## Ports Tab

Tab *Ports* is similar to the *Processes* tab, except it lists all ports found in the crashdump.

To view more details about a specific port, double-click the row or right-click it and select *Properties for <port>*. From the right-click menu, you can also select *Properties for <pid>*, where `<pid>` is the process connected to the port.

For details, see [Port Information](`p:erts:crash_dump.md#ports`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=ets_tables }
[](){: id=internal_ets_tables }
## ETS Tables Tab

Tab *ETS Tables* shows all ETS table information found in the dump. *Id* is the same as the 'Table' field in the raw crashdump. *Memory* is the 'Words' field from the raw crashdump translated into bytes. For tree tables, there is no value in the 'Objects' field.

To open the detailed information page about the table, double- click, or right-click the row and select *Properties for 'Identifier'*.

To open the detailed information page about the owner process of an ETS table, right-click the row and select *Properties for <pid>*.

For details, see [ETS Tables](`p:erts:crash_dump.md#ets_tables`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=timers }
## Timers Tab

Tab *Timers* shows all timer information found in the dump.

To open the detailed information page about the owner process of a timer, right-click the row and select *Properties for <pid>*.

Double-clicking a row in the *Timers* tab has no effect.

For details, see [Timers](`p:erts:crash_dump.md#timers`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=schedulers }
## Schedulers Tab

Tab *Schedulers* shows all scheduler information found in the dump.

To open the detailed information page about the scheduler, double-click, or right-click the row and select *Properties for 'Identifier'*.

For details, see [Scheduler Information](`p:erts:crash_dump.md#scheduler`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=funs }
## Funs Tab

Tab *Funs* shows all fun information found in the dump.

To open the detailed information page about the module to which the fun belongs, right-click the row and select *Properties for <mod>*.

Double-clicking a row in the *Funs* tab has no effect.

For details, see [Fun Information](`p:erts:crash_dump.md#funs`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=atoms }
## Atoms Tab

Tab *Atoms* lists all atoms found in the dump. By default the atoms are sorted in creation order from first to last. This is opposite of the raw crashdump where atoms are listed from last to first, meaning that if the dump was truncated in the middle of the atom list, only the last created atoms are visible in the *Atoms* tab.

For details, see [Atoms](`p:erts:crash_dump.md#atoms`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=distribution_info }
## Nodes Tab

Tab *Nodes* shows a list of all external Erlang nodes that are referenced from the crashdump.

If the page is empty, it means either of the following:

* The crashed node is not distributed.
* The crashed node is distributed but has no references to other nodes.
* The dump is truncated.

If the node is distributed, all referenced nodes are visible. Column *Connection type* shows if the node is visible, hidden, or not connected. Visible nodes are alive nodes with a living connection to the originating node. Hidden nodes are the same as visible nodes, except they are started with flag `-hidden`. Not connected nodes are nodes that are not connected to the originating node anymore, but references (that is, process or port identifiers) exist.

To see more detailed information about a node, double-click the row, or right-click the row and select *Properties for node <node>*. From the right-click menu, you can also select *Properties for <port>*, to open the detailed information window for the controlling port.

In the detailed information window for a node, any existing links and monitors between processes on the originating node and the connected node are displayed. *Extra Info* can contain debug information (that is, special information written if the emulator is debug-compiled) or error information.

For details, see [Distribution Information](`p:erts:crash_dump.md#distribution_info`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=loaded_modules }
## Modules Tab

Tab *Modules* lists all modules loaded on the originating node, and the current code size. If old code exists, the old size is also shown.

To view detailed information about a specific module, double- click the row, or right-click it and select *Properties for <mod>*.

For details, see [Loaded Module Information](`p:erts:crash_dump.md#loaded_modules`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=memory }
## Memory Tab

Tab *Memory* shows memory and allocator information. From the left-hand menu you can select the following:

* __*Memory*__ - See [Memory Information](`p:erts:crash_dump.md#memory`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

* __*Allocator Summary*__ - This page presents a summary of values from all allocators underneath it.

* __*<Allocator>*__ - One entry per allocator. See [Allocator](`p:erts:crash_dump.md#allocator`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

* __*Allocated Areas*__ - See [Allocated Areas](`p:erts:crash_dump.md#allocated_areas`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: id=internal_tables }
## Internal Tables Tab

On tab *Internal Tables* you can from the left-hand menu select *Hash Tables*, *Index Tables*, or *Internal ETS Tables*.

For details, see [Internal Table Information](`p:erts:crash_dump.md#internal_tables`) in section "How to Interpret the Erlang Crash Dumps" in ERTS.
