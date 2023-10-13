# Ports and Port Drivers

Examples of how to use ports and port drivers are provided in [Interoperability Tutorial](`p:system:tutorial.md#interoperability tutorial`). For information about the BIFs mentioned, see the `m:erlang` manual page in ERTS.

## Ports

*Ports* provide the basic mechanism for communication with the external world, from Erlang's point of view. They provide a byte-oriented interface to an external program. When a port has been created, Erlang can communicate with it by sending and receiving lists of bytes, including binaries.

The Erlang process creating a port is said to be the *port owner*, or the *connected process* of the port. All communication to and from the port must go through the port owner. If the port owner terminates, so does the port (and the external program, if it is written correctly).

The external program resides in another OS process. By default, it reads from standard input (file descriptor 0) and writes to standard output (file descriptor 1). The external program is to terminate when the port is closed.

## Port Drivers

It is possible to write a driver in C according to certain principles and dynamically link it to the Erlang runtime system. The linked-in driver looks like a port from the Erlang programmer's point of view and is called a *port driver*.

> #### Warning {: class=warning }
> An erroneous port driver causes the entire Erlang runtime system to leak memory, hang or crash.

For information about port drivers, see the [erl_driver(4)](`p:erts:erl_driver.md`) manual page in ERTS, [driver_entry(1)](`p:erts:driver_entry.md`) manual page in ERTS, and `m:erl_ddll` manual page in Kernel.

## Port BIFs

To create a port:

| `open_port(PortName, PortSettings` | Returns a port identifier `Port` as the result of opening a new Erlang port. Messages can be sent to, and received from, a port identifier, just like a pid. Port identifiers can also be linked to using `link/1`, or registered under a name using `register/2`. |
|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|


*Table: Port Creation BIF*

`PortName` is usually a tuple `{spawn,Command}`, where the string `Command` is the name of the external program. The external program runs outside the Erlang workspace, unless a port driver with the name `Command` is found. If `Command` is found, that driver is started.

`PortSettings` is a list of settings (options) for the port. The list typically contains at least a tuple `{packet,N}`, which specifies that data sent between the port and the external program are preceded by an N-byte length indicator. Valid values for N are 1, 2, or 4. If binaries are to be used instead of lists of bytes, the option `binary` must be included.

The port owner `Pid` can communicate with the port `Port` by sending and receiving messages. (In fact, any process can send the messages to the port, but the port owner must be identified in the message).

Messages sent to ports are delivered asynchronously.

> #### Change {: class=neutral }
> Before Erlang/OTP 16, messages to ports were delivered synchronously.

In the following tables of examples, `Data` must be an I/O list. An I/O list is a binary or a (possibly deep) list of binaries or integers in the range 0..255:

| *Message* | *Description* |
|-----------|---------------|
| `{Pid,{command,Data}}` | Sends `Data` to the port. |
| `{Pid,close}` | Closes the port. Unless the port is already closed, the port replies with `{Port,closed}` when all buffers have been flushed and the port really closes. |
| `{Pid,{connect,NewPid}}` | Sets the port owner of `Port` to `NewPid`. Unless the port is already closed, the port replies with`{Port,connected}` to the old port owner. Note that the old port owner is still linked to the port, but the new port owner is not. |


*Table: Messages Sent To a Port*



| *Message* | *Description* |
|-----------|---------------|
| `{Port,{data,Data}}` | `Data` is received from the external program. |
| `{Port,closed}` | Reply to `Port ! {Pid,close}`. |
| `{Port,connected}` | Reply to `Port ! {Pid,{connect,NewPid}}`. |
| `{'EXIT',Port,Reason}` | If the port has terminated for some reason. |


*Table: Messages Received From a Port*

Instead of sending and receiving messages, there are also a number of BIFs that can be used:

| *Port BIF* | *Description* |
|------------|---------------|
| `port_command(Port,Data)` | Sends `Data` to the port. |
| `port_close(Port)` | Closes the port. |
| `port_connect(Port,NewPid)` | Sets the port owner of `Port`to `NewPid`. The old port owner `Pid` stays linked to the port and must call `unlink(Port)` if this is not desired. |
| `erlang:port_info(Port,Item)` | Returns information as specified by `Item`. |
| `erlang:ports()` | Returns a list of all ports on the current node. |


*Table: Port BIFs*

Some additional BIFs that apply to port drivers: `port_control/3` and `erlang:port_call/3`.
