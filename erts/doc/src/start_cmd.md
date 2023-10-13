# start

OTP start script example for Unix.

## Description

The `start` script is an example script on how to start up the Erlang system in embedded mode on Unix.

For more information about the use, see the [Embedded System User's Guide](`p:system:embedded_solaris.md`) in System Documentation.

## start \[ data_file ]

Argument:

* __`data_file`__ - Optional. Specifies what `start_erl.data` file to use.

Environment variable `RELDIR` can be set before calling this example, which sets the directory where to find the release files.

## See Also

[`run_erl(1)`](run_erl_cmd.md), [`start_erl(1)`](start_erl_cmd.md)
