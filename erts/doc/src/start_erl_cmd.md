# start_erl

Start Erlang for embedded systems on Windows systems.

## Description

The `start_erl` program is specific to Windows NT/2000/XP (and later versions of Windows). Although there are programs with the same name on other platforms, their functionality is different.

This program is distributed both in compiled form (under <Erlang root>\\\\erts-<version>\\\\bin) and in source form (under <Erlang root>\\\\erts-<version>\\\\src). The purpose of the source code is to ease customization of the program for local needs, such as cyclic restart detection. There is also a "make"-file, written for the `nmake` program distributed with Microsoft Visual C++. This program can, however, be compiled with any Win32 C compiler (possibly with minor modifications).

This program aids release handling on Windows systems. The program is to be called by the `erlsrv` program, read up the release data file `start_erl.data`, and start Erlang. Some options to `start_erl` are added and removed by the release handler during upgrade with emulator restart (more specifically option `-data`).

## start_erl \[<erl options>] ++ \[<start_erl options>]

The `start_erl` program in its original form recognizes the following options:

* __`++`__ - Mandatory. Delimits `start_erl` options from normal Erlang options. Everything on the command line *before* `++` is interpreted as options to be sent to the `erl` program. Everything *after* `++` is interpreted as options to `start_erl` itself.

* __`-reldir <release root>`__ - Mandatory if environment variable `RELDIR` is not specified and no `-rootdir` option is specified. Tells `start_erl` where the root of the release tree is located in the file system (typically <Erlang root>\\\\releases). The `start_erl.data` file is expected to be located in this directory (unless otherwise specified). If only option `-rootdir` is specified, the directory is assumed to be <Erlang root>\\\\releases.

* __`-rootdir <Erlang root directory>`__ - Mandatory if `-reldir` is not specified and no `RELDIR` exists in the environment. This specifies the Erlang installation root directory (under which the `lib`, `releases`, and `erts-<Version>` directories are located). If only `-reldir` (or environment variable `RELDIR`) is specified, the Erlang root is assumed to be the directory exactly one level above the release directory.

* __`-data <data file name>`__ - Optional. Specifies another data file than `start_erl.data` in the <release root>. It is specified relative to the <release root> or absolute (including drive letter, and so on). This option is used by the release handler during upgrade and is not to be used during normal operation. Normally the release data file is not to be named differently.

* __`-bootflags <boot flags file name>`__ - Optional. Specifies a file name relative to the release directory (that is, the subdirectory of <release root> where the `.boot` file and others are located). The contents of this file is appended to the command line when Erlang is started. This makes it easy to start the emulator with different options for different releases.

## Notes

* As the source code is distributed, it can easily be modified to accept other options. The program must still accept option `-data` with the semantics described above for the release handler to work correctly.
* The Erlang emulator is found by examining the registry keys for the emulator version specified in the release data file. The new emulator must be properly installed before the upgrade for this to work.
* Although the program is located together with files specific to the emulator version, it is not expected to be specific to the emulator version. The release handler does *not* change option `-machine` to `erlsrv` during emulator restart. Locate the (possibly customized) `start_erl` program so that it is not overwritten during upgrade.
* The default options of the `erlsrv` program are not sufficient for release handling. The machine started by `erlsrv` is be specified as the `start_erl` program and the arguments are to contain `++` followed by the desired options.

## See Also

[`erlsrv(1)`](erlsrv_cmd.md), `m:release_handler`
