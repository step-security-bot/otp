# Getting Started

## General Information

The [start/1](`tftp:start/1`) function starts a daemon process listening for UDP packets on a port. When it receives a request for read or write, it spawns a temporary server process handling the transfer.

On the client side, function [read_file/3](`tftp:read_file/3`) and [write_file/3](`tftp:write_file/3`) spawn a temporary client process establishing contact with a TFTP daemon and perform the file transfer.

`tftp` uses a callback module to handle the file transfer. Two such callback modules are provided, `tftp_binary` and `tftp_file`. See [read_file/3](`tftp:read_file/3`) and [write_file/3](`tftp:write_file/3`) for details. You can also implement your own callback modules, see [CALLBACK FUNCTIONS](`m:tftp#tftp_callback`). A callback module provided by the user is registered using option `callback`, see [DATA TYPES](`m:tftp#options`).

## Using the TFTP client and server

This is a simple example of starting the TFTP server and reading the content of a sample file using the TFTP client.

*Step 1.* Create a sample file to be used for the transfer:

```text
      $ echo "Erlang/OTP 21" > file.txt
```

*Step 2.* Start the TFTP server:

```erlang
      1> {ok, Pid} = tftp:start([{port, 19999}]).
      {ok,<0.65.0>}
```

*Step 3.* Start the TFTP client (in another shell):

```erlang
      1> tftp:read_file("file.txt", binary, [{port, 19999}]).
      {ok,<<"Erlang/OTP 21\n">>}
```
