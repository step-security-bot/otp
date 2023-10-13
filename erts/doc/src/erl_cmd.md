# erl

The Erlang emulator.

## Description

The `erl` program starts an Erlang runtime system. The exact details (for example, whether `erl` is a script or a program and which other programs it calls) are system-dependent.

Windows users probably want to use the `werl` program instead, which runs in its own window with scrollbars and supports command-line editing. The `erl` program on Windows provides no line editing in its shell, and on Windows 95 there is no way to scroll back to text that has scrolled off the screen. The `erl` program must be used, however, in pipelines or if you want to redirect standard input or output.

> #### Note {: class=info }
> As from ERTS 5.9 (Erlang/OTP R15B) the runtime system does by default *not* bind schedulers to logical processors. For more information, see system flag [`+sbt`](erl_cmd.md#+sbt).

## erl <arguments>

Starts an Erlang runtime system.

The arguments can be divided into *emulator flags*, *flags*, and *plain arguments*:

* Any argument starting with character `+` is interpreted as an [emulator flag](erl_cmd.md#emu_flags).

  As indicated by the name, emulator flags control the behavior of the emulator.
* Any argument starting with character `-` (hyphen) is interpreted as a [flag](erl_cmd.md#init_flags), which is to be passed to the Erlang part of the runtime system, more specifically to the `init` system process, see `m:init`.

  The `init` process itself interprets some of these flags, the *init flags*. It also stores any remaining flags, the *user flags*. The latter can be retrieved by calling `init:get_argument/1`.

  A small number of "-" flags exist, which now actually are emulator flags, see the description below.
* Plain arguments are not interpreted in any way. They are also stored by the `init` process and can be retrieved by calling `init:get_plain_arguments/0`. Plain arguments can occur before the first flag, or after a `--` flag. Also, the `-extra` flag causes everything that follows to become plain arguments.

*Examples:*

```text
% erl +W w -sname arnie +R 9 -s my_init -extra +bertie
(arnie@host)1> init:get_argument(sname).
{ok,[["arnie"]]}
(arnie@host)2> init:get_plain_arguments().
["+bertie"]
```

Here `+W w` and `+R 9` are emulator flags. `-s my_init` is an init flag, interpreted by `init`. `-sname arnie` is a user flag, stored by `init`. It is read by Kernel and causes the Erlang runtime system to become distributed. Finally, everything after `-extra` (that is, `+bertie`) is considered as plain arguments.

```text
% erl -myflag 1
1> init:get_argument(myflag).
{ok,[["1"]]}
2> init:get_plain_arguments().
[]
```

Here the user flag `-myflag 1` is passed to and stored by the `init` process. It is a user-defined flag, presumably used by some user-defined application.

[](){: id=init_flags }
## Flags

In the following list, init flags are marked "(init flag)". Unless otherwise specified, all other flags are user flags, for which the values can be retrieved by calling `init:get_argument/1`. Notice that the list of user flags is not exhaustive, there can be more application-specific flags that instead are described in the corresponding application documentation.

* __`--` (init flag)__ - Everything following `--` up to the next flag (`-flag` or `+flag`) is considered plain arguments and can be retrieved using `init:get_plain_arguments/0`.

* __`-Application Par Val`__ - Sets the application configuration parameter `Par` to the value `Val` for the application `Application`; see [`app(4)`](`p:kernel:app.md`) and `m:application`.

* __`-args_file FileName`{: id=args_file }__ - Command-line arguments are read from the file `FileName`. The arguments read from the file replace flag '`-args_file FileName`' on the resulting command line.

  The file `FileName` is to be a plain text file and can contain comments and command-line arguments. A comment begins with a `#` character and continues until the next end of line character. Backslash (\\\\) is used as quoting character. All command-line arguments accepted by `erl` are allowed, also flag `-args_file FileName`. Be careful not to cause circular dependencies between files containing flag `-args_file`, though.

  The flag `-extra` is treated in special way. Its scope ends at the end of the file. Arguments following an `-extra` flag are moved on the command line into the `-extra` section, that is, the end of the command line following after an `-extra` flag.

* __`-async_shell_start`__ - The initial Erlang shell does not read user input until the system boot procedure has been completed (Erlang/OTP 5.4 and later). This flag disables the start synchronization feature and lets the shell start in parallel with the rest of the system.

* __`-boot File`{: id=boot }__ - Specifies the name of the boot file, `File.boot`, which is used to start the system; see `m:init`. Unless `File` contains an absolute path, the system searches for `File.boot` in the current and `$ROOT/bin` directories.

  Defaults to `$ROOT/bin/start.boot`.

* __`-boot_var Var Dir`__ - If the boot script contains a path variable `Var` other than `$ROOT`, this variable is expanded to `Dir`. Used when applications are installed in another directory than `$ROOT/lib`; see [`systools:make_script/1,2`](`systools:make_script/1`) in SASL.

* __`-code_path_cache`__ - Enables the code path cache of the code server; see `m:code`.

* __`-compile Mod1 Mod2 ...`__ - Compiles the specified modules and then terminates (with non-zero exit code if the compilation of some file did not succeed). Implies `-noinput`.

  Not recommended; use [`erlc`](erlc_cmd.md) instead.

* __`-config Config [Config ...]`{: id=config }__ - Specifies the name of one or more configuration files, `Config.config`, which is used to configure applications; see [`app(4)`](`p:kernel:app.md`) and `m:application`. See the documentation for the [configuration file format](`p:kernel:config.md`) for a description of the configuration format and the order in which configuration parameters are read.

* __`-configfd FD [FD ...]`{: id=configfd }__ - Specifies the name of one or more file descriptors (called configuration file descriptors from here on) with configuration data for applications; see [`app(4)`](`p:kernel:app.md`) and `m:application`. See the documentation for the [configuration file format](`p:kernel:config.md`) for a description of the configuration format and the order in which configuration parameters are read.

  A configuration file descriptor will be read until its end and will then be closed.

  The content of a configuration file descriptor is stored so that it can be reused when `init:restart/0` or `init:restart/1` is called.

  The parameter `-configfd 0` implies `-noinput`.

  > #### Note {: class=info }
  > It is not recommended to use file descriptors 1 (standard output), and 2 (standard error) together with `-configfd` as these file descriptors are typically used to print information to the console the program is running in.

  Examples (Unix shell):

  ```text
  $ erl \
  -noshell \
  -configfd 3 \
  -eval \
  'io:format("~p~n",[application:get_env(kernel, logger_level)]),erlang:halt()' 3< \
  <(echo '[{kernel, [{logger_level, warning}]}].')
  {ok,warning}
  ```

  ```text
  $ echo '[{kernel, [{logger_level, warning}]}].' > test1.config
  $ echo '[{kernel, [{logger_level, error}]}].' > test2.config
  $ erl \
  -noshell \
  -configfd 3 \
  -configfd 4 \
  -eval \
  'io:format("~p~n",[application:get_env(kernel, logger_level)]),erlang:halt()' \
  3< test1.config 4< test2.config
  {ok,error}
  ```

* __`-connect_all false`{: id=connect_all }__ - This flag is deprecated and has been replaced by the `kernel` application parameter [`connect_all`](`p:kernel:kernel_app.md#connect_all`).

* __`-cookie Cookie`__ - Obsolete flag without any effect and common misspelling for `-setcookie`. Use `-setcookie` instead.

* __`-detached`__ - Starts the Erlang runtime system detached from the system console. Useful for running daemons and backgrounds processes. Implies `-noinput`.

* __`-disable-feature feature`{: id=disable-feature }__ - Disables the [feature](`p:system:features.md#features`) `feature` in the runtime system. The special feature `all` can be used to disable all non permanent features.

* __`-dist_listen true|false`{: id=dist_listen }__ - Specifies whether this node should be listening for incoming distribution connections or not. By default a node will listen for incoming connections. Setting this option to `false` implies [`-hidden`](erl_cmd.md#hidden).

* __`-emu_args`__ - Useful for debugging. Prints the arguments sent to the emulator.

* __`-emu_flavor emu|jit|smp`__ - Start an emulator of a different flavor. Normally only one flavor is available, more can be added by building specific flavors. The currently available flavors are: `emu` and `jit`. The `smp` flavor is an alias for the current default flavor. You can combine this flag with `--emu_type`. You can get the current flavor at run-time using [`erlang:system_info(emu_flavor)`](`m:erlang#system_info_emu_flavor`). (The emulator with this flavor must be built. You can build a specific flavor by doing `make FLAVOR=$FLAVOR` in the Erlang/OTP source repository.)

* __`-emu_type Type`{: id=emu_type }__ - Start an emulator of a different type. For example, to start the lock-counter emulator, use `-emu_type lcnt`. You can get the current type at run-time using [`erlang:system_info(build_type)`](`m:erlang#system_info_emu_type`). (The emulator of this type must already be built. Use the `configure` option `--enable-lock-counter` to build the lock-counter emulator.)

* __`-enable-feature feature`{: id=enable-feature }__ - Enables the [feature](`p:system:features.md#features`) `feature` in the runtime system. The special feature `all` can be used to enable all features.

* __`-env Variable Value`__ - Sets the host OS environment variable `Variable` to the value `Value` for the Erlang runtime system. Example:

  ```text
  % erl -env DISPLAY gin:0
  ```

  In this example, an Erlang runtime system is started with environment variable `DISPLAY` set to `gin:0`.

* __`-epmd_module Module` (init flag)__ - Configures the module responsible to communicate to [epmd](epmd_cmd.md). Defaults to `erl_epmd`.

* __`-erl_epmd_port Port` (init flag)__ - Configures the port used by erl_epmd to listen for connection and connect to other nodes. See `m:erl_epmd` for more details. Defaults to `0`.

* __`-eval Expr` (init flag)__ - Makes `init` evaluate the expression `Expr`; see `m:init`.

* __`-extra` (init flag)__ - Everything following `-extra` is considered plain arguments and can be retrieved using `init:get_plain_arguments/0`.

* __`-heart`__ - Starts heartbeat monitoring of the Erlang runtime system; see `m:heart`.

* __`-hidden`{: id=hidden }__ - Starts the Erlang runtime system as a hidden node, if it is run as a distributed node. Hidden nodes always establish hidden connections to all other nodes except for nodes in the same global group. Hidden connections are not published on any of the connected nodes, that is, none of the connected nodes are part of the result from `nodes/0` on the other node. See also hidden global groups; `m:global_group`.

* __`-hosts Hosts`__ - Specifies the IP addresses for the hosts on which Erlang boot servers are running, see `m:erl_boot_server`. This flag is mandatory if flag `-loader inet` is present.

  The IP addresses must be specified in the standard form (four decimal numbers separated by periods, for example, `"150.236.20.74"`. Hosts names are not acceptable, but a broadcast address (preferably limited to the local network) is.

* __`-id Id`__ - Specifies the identity of the Erlang runtime system. If it is run as a distributed node, `Id` must be identical to the name supplied together with flag `-sname` or `-name`.

* __`-init_debug`{: id=init_debug }__ - Makes `init` write some debug information while interpreting the boot script.

* __`-instr`{: id=instr } (emulator flag)__ - Selects an instrumented Erlang runtime system (virtual machine) to run, instead of the ordinary one. When running an instrumented runtime system, some resource usage data can be obtained and analyzed using the `instrument` module. Functionally, it behaves exactly like an ordinary Erlang runtime system.

* __`-loader Loader`__ - Specifies the method used by `erl_prim_loader` to load Erlang modules into the system; see `m:erl_prim_loader`. Two `Loader` methods are supported:

  * `efile`, which means use the local file system, this is the default.
  * `inet`, which means use a boot server on another machine. The flags `-id`, `-hosts` and `-setcookie` must also be specified.

  If `Loader` is something else, the user-supplied `Loader` port program is started.

* __`-make`__ - Makes the Erlang runtime system invoke `make:all()` in the current working directory and then terminate; see `m:make`. Implies `-noinput`.

* __`-man Module`__ - Displays the manual page for the Erlang module `Module`. Only supported on Unix.

* __`-mode interactive | embedded`__ - Modules are auto loaded when they are first referenced if the runtime system runs in `interactive` mode, which is the default. In `embedded` mode modules are not auto loaded. The latter is recommended when the boot script preloads all modules, as conventionally happens in OTP releases. See `m:code`.

* __`-name Name`{: id=name }__ - Makes the Erlang runtime system into a distributed node. This flag invokes all network servers necessary for a node to become distributed; see `m:net_kernel`. It also ensures that `epmd` runs on the current host before Erlang is started (see [`epmd(1)`](epmd_cmd.md) and the [`-start_epmd`](erl_cmd.md#start_epmd) option) and that a magic cookie has been set (see [\-setcookie](erl_cmd.md#setcookie)).

  The node name will be `Name@Host`, where `Host` is the fully qualified host name of the current host. For short names, use flag `-sname` instead.

  If `Name` is set to *`undefined`* the node will be started in a special mode optimized to be the temporary client of another node. The node will then request a dynamic node name from the first node it connects to. Read more in [Dynamic Node Name](`p:system:distributed.md#dyn_node_name`).

  > #### Warning {: class=warning }
  > Starting a distributed node without also specifying [`-proto_dist inet_tls`](erl_cmd.md#proto_dist) will expose the node to attacks that may give the attacker complete access to the node and in extension the cluster. When using un-secure distributed nodes, make sure that the network is configured to keep potential attackers out.

* __`-no_epmd`__ - Specifies that the distributed node does not need [epmd](epmd_cmd.md) at all.

  This option ensures that the Erlang runtime system does not start [epmd](epmd_cmd.md) and does not start the `m:erl_epmd` process for distribution either.

  This option only works if Erlang is started as a distributed node with the [\-proto_dist](erl_cmd.md#proto_dist) option using an alternative protocol for Erlang distribution which does not rely on epmd for node registration and discovery. For more information, see [How to implement an Alternative Carrier for the Erlang Distribution](alt_dist.md).

* __`-noinput`{: id=noinput }__ - Ensures that the Erlang runtime system never tries to read any input. Implies `-noshell`.

* __`-noshell`{: id=noshell }__ - Starts an Erlang runtime system with no shell. This flag makes it possible to have the Erlang runtime system as a component in a series of Unix pipes.

* __`-nostick`__ - Disables the sticky directory facility of the Erlang code server; see `m:code`.

* __`-oldshell`__ - Invokes the old Erlang shell from Erlang/OTP 3.3. The old shell can still be used.

* __`-pa Dir1 Dir2 ...`__ - Adds the specified directories to the beginning of the code path, similar to `code:add_pathsa/1`. Note that the order of the given directories will be reversed in the resulting path.

  As an alternative to `-pa`, if several directories are to be prepended to the code path and the directories have a common parent directory, that parent directory can be specified in environment variable `ERL_LIBS`; see `m:code`.

* __`-pz Dir1 Dir2 ...`__ - Adds the specified directories to the end of the code path, similar to `code:add_pathsz/1`; see `m:code`.

* __`-path Dir1 Dir2 ...`__ - Replaces the path specified in the boot script; see [`script(4)`](`p:sasl:script.md`).

* __`-proto_dist Proto`__ - [](){: id=proto_dist }
  Specifies a protocol for Erlang distribution:

  * __`inet_tcp`__ - TCP over IPv4 (the default)

  * __`inet_tls`__ - Distribution over TLS/SSL, See the [Using SSL for Erlang Distribution](`p:ssl:ssl_distribution.md`) User's Guide for details on how to setup a secure distributed node.

  * __`inet6_tcp`__ - TCP over IPv6

  For example, to start up IPv6 distributed nodes:

  ```text
  % erl -name test@ipv6node.example.com -proto_dist inet6_tcp
  ```

* __`-remsh Node`{: id=remsh }__ - Starts Erlang with a remote shell connected to `Node`.

  If no `-name` or `-sname` is given the node will be started using `-sname undefined`. If `Node` does not contain a hostname, one is automatically taken from `-name` or `-sname`

  > #### Note {: class=info }
  > Before OTP-23 the user *needed* to supply a valid `-sname` or `-name` for `-remsh` to work. This is still the case if the target node is not running OTP-23 or later.

  > #### Note {: class=info }
  > The connecting node needs to have a proper shell with terminal emulation. This means that UNIX users must use an Erlang compiled with terminal capabilities and Windows users must use [`werl(1)`](werl_cmd.md).

* __`-rsh Program`__ - Specifies an alternative to `ssh` for starting a slave node on a remote host; see `m:slave`.

* __`-S Mod [Func [Arg1, Arg2, ...]]` (init flag)__ - Makes `init` call the specified function. `Func` defaults to `start`. The function is assumed to be of arity 1, taking the list `[Arg1,Arg2,...]` as argument, or an empty list if no arguments are passed. All further arguments occurring after this option are passed to the specified function as strings. See `m:init`.

* __`-run Mod [Func [Arg1, Arg2, ...]]` (init flag)__ - Makes `init` call the specified function. `Func` defaults to `start`. If no arguments are provided, the function is assumed to be of arity 0. Otherwise it is assumed to be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments are passed as strings. See `m:init`.

* __`-s Mod [Func [Arg1, Arg2, ...]]` (init flag)__ - Makes `init` call the specified function. `Func` defaults to `start`. If no arguments are provided, the function is assumed to be of arity 0. Otherwise it is assumed to be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments are passed as atoms. See `m:init`.

* __`-setcookie Cookie`{: id=setcookie }__ - Sets the magic cookie of the node to `Cookie`; see `erlang:set_cookie/1`. See see section [Distributed Erlang](`p:system:distributed.md`) in the Erlang Reference Manual for more details.

* __`-setcookie Node Cookie`__ - Sets the magic cookie for `Node` to `Cookie`; see `erlang:set_cookie/2`.

* __`-shutdown_time Time`__ - Specifies how long time (in milliseconds) the `init` process is allowed to spend shutting down the system. If `Time` milliseconds have elapsed, all processes still existing are killed. Defaults to `infinity`.

* __`-sname Name`{: id=sname }__ - Makes the Erlang runtime system into a distributed node, similar to [`-name`](erl_cmd.md#name), but the host name portion of the node name `Name@Host` will be the short name, not fully qualified.

  This is sometimes the only way to run distributed Erlang if the Domain Name System (DNS) is not running. No communication can exist between nodes running with flag `-sname` and those running with flag `-name`, as node names must be unique in distributed Erlang systems.

  If `Name` is set to *`undefined`* the node will be started in a special mode optimized to be the temporary client of another node. The node will then request a dynamic node name from the first node it connects to. Read more in [Dynamic Node Name](`p:system:distributed.md#dyn_node_name`).

  > #### Warning {: class=warning }
  > Starting a distributed node without also specifying [`-proto_dist inet_tls`](erl_cmd.md#proto_dist) will expose the node to attacks that may give the attacker complete access to the node and in extension the cluster. When using un-secure distributed nodes, make sure that the network is configured to keep potential attackers out.

* __`-start_epmd true | false`{: id=start_epmd }__ - Specifies whether Erlang should start [epmd](epmd_cmd.md) on startup. By default this is `true`, but if you prefer to start epmd manually, set this to `false`.

  This only applies if Erlang is started as a distributed node, i.e. if `-name` or `-sname` is specified. Otherwise, epmd is not started even if `-start_epmd true` is given.

  Note that a distributed node will fail to start if epmd is not running.

* __`-version` (emulator flag)__ - Makes the emulator print its version number. The same as `erl +V`.

[](){: id=emu_flags }
## Emulator Flags

`erl` invokes the code for the Erlang emulator (virtual machine), which supports the following flags:

* __`+a size`{: id=async_thread_stack_size }__ - Suggested stack size, in kilowords, for threads in the async thread pool. Valid range is 16-8192 kilowords. The default suggested stack size is 16 kilowords, that is, 64 kilobyte on 32-bit architectures. This small default size has been chosen because the number of async threads can be large. The default size is enough for drivers delivered with Erlang/OTP, but might not be large enough for other dynamically linked-in drivers that use the [`driver_async()`](erl_driver.md#driver_async) functionality. Notice that the value passed is only a suggestion, and it can even be ignored on some platforms.

* __`+A size`{: id=async_thread_pool_size }__ - Sets the number of threads in async thread pool. Valid range is 1-1024. The async thread pool is used by linked-in drivers to handle work that may take a very long time. Since OTP 21 there are very few linked-in drivers in the default Erlang/OTP distribution that uses the async thread pool. Most of them have been migrated to dirty IO schedulers. Defaults to 1.

* __`+B [c | d | i]`__ - Option `c` makes `Ctrl-C` interrupt the current shell instead of invoking the emulator break handler. Option `d` (same as specifying `+B` without an extra option) disables the break handler. Option `i` makes the emulator ignore any break signal.

  If option `c` is used with `oldshell` on Unix, `Ctrl-C` will restart the shell process rather than interrupt it.

  Notice that on Windows, this flag is only applicable for `werl`, not `erl` (`oldshell`). Notice also that `Ctrl-Break` is used instead of `Ctrl-C` on Windows.

* __`+c true | false`{: id=+c }__ - Enables or disables [time correction](time_correction.md#time_correction):

  * __`true`__ - Enables time correction. This is the default if time correction is supported on the specific platform.

  * __`false`__ - Disables time correction.

  For backward compatibility, the boolean value can be omitted. This is interpreted as `+c false`.

* __`+C no_time_warp | single_time_warp | multi_time_warp`{: id=+C_ }__ - Sets [time warp mode](time_correction.md#time_warp_modes):

  * __`no_time_warp`__ - [No time warp mode](time_correction.md#no_time_warp_mode) (the default)

  * __`single_time_warp`__ - [Single time warp mode](time_correction.md#single_time_warp_mode)

  * __`multi_time_warp`__ - [Multi-time warp mode](time_correction.md#multi_time_warp_mode)

  

* __`+d`__ - If the emulator detects an internal error (or runs out of memory), it, by default, generates both a crash dump and a core dump. The core dump is, however, not very useful as the content of process heaps is destroyed by the crash dump generation.

  Option `+d` instructs the emulator to produce only a core dump and no crash dump if an internal error is detected.

  Calling `erlang:halt/1` with a string argument still produces a crash dump. On Unix systems, sending an emulator process a `SIGUSR1` signal also forces a crash dump.

* __`+dcg DecentralizedCounterGroupsLimit`{: id=+dcg }__ - Limits the number of decentralized counter groups used by decentralized counters optimized for update operations in the Erlang runtime system. By default, the limit is 256.

  When the number of schedulers is less than or equal to the limit, each scheduler has its own group. When the number of schedulers is larger than the groups limit, schedulers share groups. Shared groups degrade the performance for updating counters while many reader groups degrade the performance for reading counters. So, the limit is a tradeoff between performance for update operations and performance for read operations. Each group consumes 64 bytes in each counter.

  Notice that a runtime system using decentralized counter groups benefits from [binding schedulers to logical processors](erl_cmd.md#+sbt), as the groups are distributed better between schedulers with this option.

  This option only affects decentralized counters used for the counters that are keeping track of the memory consumption and the number of terms in ETS tables of type ordered_set with the write_concurrency option activated.

* __`+e Number`{: id=+e }__ - Sets the maximum number of ETS tables. This limit is [partially obsolete](`m:ets#max_ets_tables`).

* __`+ec`__ - Forces option `compressed` on all ETS tables. Only intended for test and evaluation.

* __`+fnl`{: id=file_name_encoding }__ - The virtual machine works with filenames as if they are encoded using the ISO Latin-1 encoding, disallowing Unicode characters with code points > 255.

  For more information about Unicode filenames, see section [Unicode Filenames](`p:stdlib:unicode_usage.md#unicode_file_names`) in the STDLIB User's Guide. Notice that this value also applies to command-line parameters and environment variables (see section [Unicode in Environment and Parameters](`p:stdlib:unicode_usage.md#unicode_in_environment_and_parameters`) in the STDLIB User's Guide).

* __`+fnu[{w|i|e}]`__ - The virtual machine works with filenames as if they are encoded using UTF-8 (or some other system-specific Unicode encoding). This is the default on operating systems that enforce Unicode encoding, that is, Windows MacOS X and Android.

  The `+fnu` switch can be followed by `w`, `i`, or `e` to control how wrongly encoded filenames are to be reported:

  * `w` means that a warning is sent to the `error_logger` whenever a wrongly encoded filename is "skipped" in directory listings. This is the default.
  * `i` means that those wrongly encoded filenames are silently ignored.
  * `e` means that the API function returns an error whenever a wrongly encoded filename (or directory name) is encountered.

  Notice that `file:read_link/1` always returns an error if the link points to an invalid filename.

  For more information about Unicode filenames, see section [Unicode Filenames](`p:stdlib:unicode_usage.md#unicode_file_names`) in the STDLIB User's Guide. Notice that this value also applies to command-line parameters and environment variables (see section [Unicode in Environment and Parameters](`p:stdlib:unicode_usage.md#unicode_in_environment_and_parameters`) in the STDLIB User's Guide).

* __`+fna[{w|i|e}]`__ - Selection between `+fnl` and `+fnu` is done based on the current locale settings in the OS. This means that if you have set your terminal for UTF-8 encoding, the filesystem is expected to use the same encoding for filenames. This is the default on all operating systems, except Android, MacOS X and Windows.

  The `+fna` switch can be followed by `w`, `i`, or `e`. This has effect if the locale settings cause the behavior of `+fnu` to be selected; see the description of `+fnu` above. If the locale settings cause the behavior of `+fnl` to be selected, then `w`, `i`, or `e` have no effect.

  For more information about Unicode filenames, see section [Unicode Filenames](`p:stdlib:unicode_usage.md#unicode_file_names`) in the STDLIB User's Guide. Notice that this value also applies to command-line parameters and environment variables (see section [Unicode in Environment and Parameters](`p:stdlib:unicode_usage.md#unicode_in_environment_and_parameters`) in the STDLIB User's Guide).

* __`+hms Size`__ - Sets the default heap size of processes to the size `Size` words.

* __`+hmbs Size`__ - Sets the default binary virtual heap size of processes to the size `Size` words.

* __`+hmax Size`{: id=+hmax }__ - Sets the default maximum heap size of processes to the size `Size` words. Defaults to `0`, which means that no maximum heap size is used. For more information, see [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).

* __`+hmaxel true|false`{: id=+hmaxel }__ - Sets whether to send an error logger message or not for processes reaching the maximum heap size. Defaults to `true`. For more information, see [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).

* __`+hmaxib true|false`{: id=+hmaxib }__ - Sets whether to include the size of shared off-heap binaries in the sum compared against the maximum heap size. Defaults to `false`. For more information, see [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).

* __`+hmaxk true|false`{: id=+hmaxk }__ - Sets whether to kill processes reaching the maximum heap size or not. Default to `true`. For more information, see [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).

* __`+hpds Size`__ - Sets the initial process dictionary size of processes to the size `Size`.

* __`+hmqd off_heap|on_heap`{: id=+hmqd }__ - Sets the default value of the `message_queue_data` process flag. Defaults to `on_heap`. If `+hmqd` is not passed, `on_heap` will be the default. For more information, see [`process_flag(message_queue_data, MQD)`](`m:erlang#process_flag_message_queue_data`).

* __`+IOp PollSets`{: id=+IOp }__ - Sets the number of IO pollsets to use when polling for I/O. This option is only used on platforms that support concurrent updates of a pollset, otherwise the same number of pollsets are used as IO poll threads. The default is 1.

* __`+IOt PollThreads`{: id=+IOt }__ - Sets the number of IO poll threads to use when polling for I/O. The maximum number of poll threads allowed is 1024. The default is 1.

  A good way to check if more IO poll threads are needed is to use [microstate accounting](`m:msacc`) and see what the load of the IO poll thread is. If it is high it could be a good idea to add more threads.

* __`+IOPp PollSetsPercentage`{: id=+IOPp }__ - Similar to [`+IOp`](erl_cmd.md#+iop) but uses percentages to set the number of IO pollsets to create, based on the number of poll threads configured. If both `+IOPp` and `+IOp` are used, `+IOPp` is ignored.

* __`+IOPt PollThreadsPercentage`{: id=+IOPt }__ - Similar to [`+IOt`](erl_cmd.md#+iot) but uses percentages to set the number of IO poll threads to create, based on the number of schedulers configured. If both `+IOPt` and `+IOt` are used, `+IOPt` is ignored.

* __`+IOs true|false`{: id=+IOs }__ - Enable or disable scheduler thread poll optimization. Default is `true`.

  If enabled, file descriptors that are frequently read may be moved to a special pollset used by scheduler threads. The objective is to reduce the number of system calls and thereby CPU load, but it can in some cases increase scheduling latency for individual file descriptor input events.

* __`+JPperf true|false|dump|map|fp|no_fp`{: id=+JPperf }__ - Enables or disables support for the `perf` profiler when running with the JIT on Linux. Defaults to false.

  This option can be combined multiple times to enable several options:

  * __`dump`__ - Gives `perf` detailed line information, so that the `perf annotate` feature works.

  * __`map`__ - Gives `perf` a map over all module code, letting it translate machine code addresses to Erlang source code locations. This also enables frame pointers for Erlang code so that `perf` can walk the call stacks of Erlang processes, which costs one extra word per stack frame.

  * __`fp`__ - Enables frame pointers independently of the `map` option.

  * __`no_fp`__ - Disables the frame pointers added by the `map` option.

  * __`true`__ - Enables `map` and `dump`.

  * __`false`__ - Disables all other options.

  For more details about how to run perf see the [perf support](beamasm.md#linux-perf-support) section in the BeamAsm internal documentation.

* __`+JMsingle true|false`{: id=+JMsingle }__ - Since: OTP-26.0

  Enables or disables the use of single-mapped RWX memory for JIT code. The default is to map JIT:ed machine code into two regions sharing the same physical pages, where one region is executable but not writable, and the other writable but not executable. As some tools, such as QEMU user mode emulation, cannot deal with the dual mapping, this flags allows it to be disabled. This flag is automatically enabled by the [`+JPperf`](erl_cmd.md#+jpperf) flag.

* __`+L`__ - Prevents loading information about source filenames and line numbers. This saves some memory, but exceptions do not contain information about the filenames and line numbers.

* __`+MFlag Value`{: id=erts_alloc }__ - Memory allocator-specific flags. For more information, see [`erts_alloc(3)`](erts_alloc.md).

* __`+pad true|false`{: id=+pad }__ - Since: OTP 25.3

  The boolean value used with the `+pad` parameter determines the default value of the [`async_dist`](`m:erlang#process_flag_async_dist`) process flag of newly spawned processes. By default, if no `+pad` command line option is passed, the `async_dist` flag will be set to `false`.

  The value used in runtime can be inspected by calling [`erlang:system_info(async_dist)`](`m:erlang#system_info_async_dist`).

* __[](){: id=+pc }
  `+pc Range`{: id=printable_character_range }__  
  Sets the range of characters that the system considers printable in heuristic detection of strings. This typically affects the shell, debugger, and `io:format` functions (when `~tp` is used in the format string).

  Two values are supported for `Range`:

  * __`latin1`__ - The default. Only characters in the ISO Latin-1 range can be considered printable. This means that a character with a code point > 255 is never considered printable and that lists containing such characters are displayed as lists of integers rather than text strings by tools.

  * __`unicode`__ - All printable Unicode characters are considered when determining if a list of integers is to be displayed in string syntax. This can give unexpected results if, for example, your font does not cover all Unicode characters.

  See also `io:printable_range/0` in STDLIB.

* __[](){: id=+P }
  `+P Number`{: id=max_processes }__  
  Sets the maximum number of simultaneously existing processes for this system if a `Number` is passed as value. Valid range for `Number` is `[1024-134217727]`

  *NOTE*: The actual maximum chosen may be much larger than the `Number` passed. Currently the runtime system often, but not always, chooses a value that is a power of 2. This might, however, be changed in the future. The actual value chosen can be checked by calling [erlang:system_info(process_limit)](`m:erlang#system_info_process_limit`).

  The default value is `1048576`

* __[](){: id=+Q }
  `+Q Number`{: id=max_ports }__  
  Sets the maximum number of simultaneously existing ports for this system if a Number is passed as value. Valid range for `Number` is `[1024-134217727]`

  *NOTE*: The actual maximum chosen may be much larger than the actual `Number` passed. Currently the runtime system often, but not always, chooses a value that is a power of 2. This might, however, be changed in the future. The actual value chosen can be checked by calling [erlang:system_info(port_limit)](`m:erlang#system_info_port_limit`).

  The default value used is normally `65536`. However, if the runtime system is able to determine maximum amount of file descriptors that it is allowed to open and this value is larger than `65536`, the chosen value will increased to a value larger or equal to the maximum amount of file descriptors that can be opened.

  On Windows the default value is set to `8196` because the normal OS limitations are set higher than most machines can handle.

* __`+R ReleaseNumber`{: id=compat_rel }__ - Sets the compatibility mode.

  The distribution mechanism is not backward compatible by default. This flag sets the emulator in compatibility mode with an earlier Erlang/OTP release `ReleaseNumber`. The release number must be in the range `<current release>-2..<current release>`. This limits the emulator, making it possible for it to communicate with Erlang nodes (as well as C- and Java nodes) running that earlier release.

  > #### Note {: class=info }
  > Ensure that all nodes (Erlang-, C-, and Java nodes) of a distributed Erlang system is of the same Erlang/OTP release, or from two different Erlang/OTP releases X and Y, where *all* Y nodes have compatibility mode X.

* __`+r`__ - Forces ETS memory block to be moved on realloc.

* __`+rg ReaderGroupsLimit`{: id=+rg }__ - Limits the number of reader groups used by read/write locks optimized for read operations in the Erlang runtime system. By default the reader groups limit is 64.

  When the number of schedulers is less than or equal to the reader groups limit, each scheduler has its own reader group. When the number of schedulers is larger than the reader groups limit, schedulers share reader groups. Shared reader groups degrade read lock and read unlock performance while many reader groups degrade write lock performance. So, the limit is a tradeoff between performance for read operations and performance for write operations. Each reader group consumes 64 byte in each read/write lock.

  Notice that a runtime system using shared reader groups benefits from [binding schedulers to logical processors](erl_cmd.md#+sbt), as the reader groups are distributed better between schedulers.

* __`+S Schedulers:SchedulerOnline`{: id=+S }__ - Sets the number of scheduler threads to create and scheduler threads to set online. The maximum for both values is 1024. If the Erlang runtime system is able to determine the number of logical processors configured and logical processors available, `Schedulers` defaults to logical processors configured, and `SchedulersOnline` defaults to logical processors available; otherwise the default values are 1. If the emulator detects that it is subject to a [CPU quota](`m:erlang#system_info_cpu_quota`), the default value for `SchedulersOnline` will be limited accordingly.

  `Schedulers` can be omitted if `:SchedulerOnline` is not and conversely. The number of schedulers online can be changed at runtime through [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).

  If `Schedulers` or `SchedulersOnline` is specified as a negative number, the value is subtracted from the default number of logical processors configured or logical processors available, respectively.

  Specifying value `0` for `Schedulers` or `SchedulersOnline` resets the number of scheduler threads or scheduler threads online, respectively, to its default value.

* __`+SP SchedulersPercentage:SchedulersOnlinePercentage`{: id=+SP }__ - Similar to [`+S`](erl_cmd.md#+s) but uses percentages to set the number of scheduler threads to create, based on logical processors configured, and scheduler threads to set online, based on logical processors available. Specified values must be > 0. For example, `+SP 50:25` sets the number of scheduler threads to 50% of the logical processors configured, and the number of scheduler threads online to 25% of the logical processors available. `SchedulersPercentage` can be omitted if `:SchedulersOnlinePercentage` is not and conversely. The number of schedulers online can be changed at runtime through [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).

  This option interacts with [`+S`](erl_cmd.md#+s) settings. For example, on a system with 8 logical cores configured and 8 logical cores available, the combination of the options `+S 4:4 +SP 50:25` (in either order) results in 2 scheduler threads (50% of 4) and 1 scheduler thread online (25% of 4).

* __`+SDcpu DirtyCPUSchedulers:DirtyCPUSchedulersOnline`{: id=+SDcpu }__ - Sets the number of dirty CPU scheduler threads to create and dirty CPU scheduler threads to set online. The maximum for both values is 1024, and each value is further limited by the settings for normal schedulers:

  * The number of dirty CPU scheduler threads created cannot exceed the number of normal scheduler threads created.
  * The number of dirty CPU scheduler threads online cannot exceed the number of normal scheduler threads online.

  For details, see the [`+S`](erl_cmd.md#+s) and [`+SP`](erl_cmd.md#+sp). By default, the number of dirty CPU scheduler threads created equals the number of normal scheduler threads created, and the number of dirty CPU scheduler threads online equals the number of normal scheduler threads online. `DirtyCPUSchedulers` can be omitted if `:DirtyCPUSchedulersOnline` is not and conversely. The number of dirty CPU schedulers online can be changed at runtime through [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).

  The amount of dirty CPU schedulers is limited by the amount of normal schedulers in order to limit the effect on processes executing on ordinary schedulers. If the amount of dirty CPU schedulers was allowed to be unlimited, dirty CPU bound jobs would potentially starve normal jobs.

  Typical users of the dirty CPU schedulers are large garbage collections, json protocol encode/decoders written as nifs and matrix manipulation libraries.

  You can use `m:msacc` in order to see the current load of the dirty CPU schedulers threads and adjust the number used accordingly.

* __`+SDPcpu DirtyCPUSchedulersPercentage:DirtyCPUSchedulersOnlinePercentage`{: id=+SDPcpu }__ - Similar to [`+SDcpu`](erl_cmd.md#+sdcpu) but uses percentages to set the number of dirty CPU scheduler threads to create and the number of dirty CPU scheduler threads to set online. Specified values must be > 0. For example, `+SDPcpu 50:25` sets the number of dirty CPU scheduler threads to 50% of the logical processors configured and the number of dirty CPU scheduler threads online to 25% of the logical processors available. `DirtyCPUSchedulersPercentage` can be omitted if `:DirtyCPUSchedulersOnlinePercentage` is not and conversely. The number of dirty CPU schedulers online can be changed at runtime through [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).

  This option interacts with [`+SDcpu`](erl_cmd.md#+sdcpu) settings. For example, on a system with 8 logical cores configured and 8 logical cores available, the combination of the options `+SDcpu 4:4 +SDPcpu 50:25` (in either order) results in 2 dirty CPU scheduler threads (50% of 4) and 1 dirty CPU scheduler thread online (25% of 4).

* __`+SDio DirtyIOSchedulers`{: id=+SDio }__ - Sets the number of dirty I/O scheduler threads to create. Valid range is 1-1024. By default, the number of dirty I/O scheduler threads created is 10.

  The amount of dirty IO schedulers is not limited by the amount of normal schedulers [like the amount of dirty CPU schedulers](erl_cmd.md#+sdcpu). This since only I/O bound work is expected to execute on dirty I/O schedulers. If the user should schedule CPU bound jobs on dirty I/O schedulers, these jobs might starve ordinary jobs executing on ordinary schedulers.

  Typical users of the dirty IO schedulers are reading and writing to files.

  You can use `m:msacc` in order to see the current load of the dirty IO schedulers threads and adjust the number used accordingly.

* __`+sFlag Value`__ - Scheduling specific flags.

  * __`+sbt BindType`{: id=+sbt }__ - Sets scheduler bind type.

    Schedulers can also be bound using flag [`+stbt`](erl_cmd.md#+stbt). The only difference between these two flags is how the following errors are handled:

    * Binding of schedulers is not supported on the specific platform.
    * No available CPU topology. That is, the runtime system was not able to detect the CPU topology automatically, and no [user-defined CPU topology](erl_cmd.md#+sct) was set.

    If any of these errors occur when `+sbt` has been passed, the runtime system prints an error message, and refuses to start. If any of these errors occur when `+stbt` has been passed, the runtime system silently ignores the error, and start up using unbound schedulers.

    Valid `BindType`s:

    * __`u`__ - `unbound` \- Schedulers are not bound to logical processors, that is, the operating system decides where the scheduler threads execute, and when to migrate them. This is the default.

    * __`ns`__ - `no_spread` \- Schedulers with close scheduler identifiers are bound as close as possible in hardware.

    * __`ts`__ - `thread_spread` \- Thread refers to hardware threads (such as Intel's hyper-threads). Schedulers with low scheduler identifiers, are bound to the first hardware thread of each core, then schedulers with higher scheduler identifiers are bound to the second hardware thread of each core,and so on.

    * __`ps`__ - `processor_spread` \- Schedulers are spread like `thread_spread`, but also over physical processor chips.

    * __`s`__ - `spread` \- Schedulers are spread as much as possible.

    * __`nnts`__ - `no_node_thread_spread` \- Like `thread_spread`, but if multiple Non-Uniform Memory Access (NUMA) nodes exist, schedulers are spread over one NUMA node at a time, that is, all logical processors of one NUMA node are bound to schedulers in sequence.

    * __`nnps`__ - `no_node_processor_spread` \- Like `processor_spread`, but if multiple NUMA nodes exist, schedulers are spread over one NUMA node at a time, that is, all logical processors of one NUMA node are bound to schedulers in sequence.

    * __`tnnps`__ - `thread_no_node_processor_spread` \- A combination of `thread_spread`, and `no_node_processor_spread`. Schedulers are spread over hardware threads across NUMA nodes, but schedulers are only spread over processors internally in one NUMA node at a time.

    * __`db`__ - `default_bind` \- Binds schedulers the default way. Defaults to `thread_no_node_processor_spread` (which can change in the future).

    Binding of schedulers is only supported on newer Linux, Solaris, FreeBSD, and Windows systems.

    If no CPU topology is available when flag `+sbt` is processed and `BindType` is any other type than `u`, the runtime system fails to start. CPU topology can be defined using flag [`+sct`](erl_cmd.md#+sct). Notice that flag `+sct` can have to be passed before flag `+sbt` on the command line (if no CPU topology has been automatically detected).

    The runtime system does by default *not* bind schedulers to logical processors.

    > #### Note {: class=info }
    > If the Erlang runtime system is the only operating system process that binds threads to logical processors, this improves the performance of the runtime system. However, if other operating system processes (for example another Erlang runtime system) also bind threads to logical processors, there can be a performance penalty instead. This performance penalty can sometimes be severe. If so, you are advised not to bind the schedulers.

    How schedulers are bound matters. For example, in situations when there are fewer running processes than schedulers online, the runtime system tries to migrate processes to schedulers with low scheduler identifiers. The more the schedulers are spread over the hardware, the more resources are available to the runtime system in such situations.

    > #### Note {: class=info }
    > If a scheduler fails to bind, this is often silently ignored, as it is not always possible to verify valid logical processor identifiers. If an error is reported, it is reported to the `error_logger`. If you want to verify that the schedulers have bound as requested, call [`erlang:system_info(scheduler_bindings)`](`m:erlang#system_info_scheduler_bindings`).

  * __`+sbwt none|very_short|short|medium|long|very_long`{: id=+sbwt }__ - Sets scheduler busy wait threshold. Defaults to `medium`. The threshold determines how long schedulers are to busy wait when running out of work before going to sleep.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+sbwtdcpu none|very_short|short|medium|long|very_long`{: id=+sbwtdcpu }__ - As [`+sbwt`](erl_cmd.md#+sbwt) but affects dirty CPU schedulers. Defaults to `short`.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+sbwtdio none|very_short|short|medium|long|very_long`{: id=+sbwtdio }__ - As [`+sbwt`](erl_cmd.md#+sbwt) but affects dirty IO schedulers. Defaults to `short`.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+scl true|false`{: id=+scl }__ - Enables or disables scheduler compaction of load. By default scheduler compaction of load is enabled. When enabled, load balancing strives for a load distribution, which causes as many scheduler threads as possible to be fully loaded (that is, not run out of work). This is accomplished by migrating load (for example, runnable processes) into a smaller set of schedulers when schedulers frequently run out of work. When disabled, the frequency with which schedulers run out of work is not taken into account by the load balancing logic.

    `+scl false` is similar to [`+sub true`](erl_cmd.md#+sub), but `+sub true` also balances scheduler utilization between schedulers.

  * __`+sct CpuTopology`{: id=+sct }__ - * `<Id> = integer(); when 0 =< <Id> =< 65535`
    * `<IdRange> = <Id>-<Id>`
    * `<IdOrIdRange> = <Id> | <IdRange>`
    * `<IdList> = <IdOrIdRange>,<IdOrIdRange> | <IdOrIdRange>`
    * `<LogicalIds> = L<IdList>`
    * `<ThreadIds> = T<IdList> | t<IdList>`
    * `<CoreIds> = C<IdList> | c<IdList>`
    * `<ProcessorIds> = P<IdList> | p<IdList>`
    * `<NodeIds> = N<IdList> | n<IdList>`
    * `<IdDefs> = <LogicalIds><ThreadIds><CoreIds><ProcessorIds><NodeIds> | <LogicalIds><ThreadIds><CoreIds><NodeIds><ProcessorIds>`
    * `CpuTopology = <IdDefs>:<IdDefs> | <IdDefs>`

    Sets a user-defined CPU topology. The user-defined CPU topology overrides any automatically detected CPU topology. The CPU topology is used when [binding schedulers to logical processors](erl_cmd.md#+sbt).

    Uppercase letters signify real identifiers and lowercase letters signify fake identifiers only used for description of the topology. Identifiers passed as real identifiers can be used by the runtime system when trying to access specific hardware; if they are incorrect the behavior is undefined. Faked logical CPU identifiers are not accepted, as there is no point in defining the CPU topology without real logical CPU identifiers. Thread, core, processor, and node identifiers can be omitted. If omitted, the thread ID defaults to `t0`, the core ID defaults to `c0`, the processor ID defaults to `p0`, and the node ID is left undefined. Either each logical processor must belong to only one NUMA node, or no logical processors must belong to any NUMA nodes.

    Both increasing and decreasing `<IdRange>`s are allowed.

    NUMA node identifiers are system wide. That is, each NUMA node on the system must have a unique identifier. Processor identifiers are also system wide. Core identifiers are processor wide. Thread identifiers are core wide.

    The order of the identifier types implies the hierarchy of the CPU topology. The valid orders are as follows:

    * `<LogicalIds><ThreadIds><CoreIds><ProcessorIds><NodeIds>`, that is, thread is part of a core that is part of a processor, which is part of a NUMA node.
    * `<LogicalIds><ThreadIds><CoreIds><NodeIds><ProcessorIds>`, that is, thread is part of a core that is part of a NUMA node, which is part of a processor.

    A CPU topology can consist of both processor external, and processor internal NUMA nodes as long as each logical processor belongs to only one NUMA node. If `<ProcessorIds>` is omitted, its default position is before `<NodeIds>`. That is, the default is processor external NUMA nodes.

    If a list of identifiers is used in an `<IdDefs>`:

    * `<LogicalIds>` must be a list of identifiers.
    * At least one other identifier type besides `<LogicalIds>` must also have a list of identifiers.
    * All lists of identifiers must produce the same number of identifiers.

    A simple example. A single quad core processor can be described as follows:

    ```text
    % erl +sct L0-3c0-3
    1> erlang:system_info(cpu_topology).
    [{processor,[{core,{logical,0}},
                 {core,{logical,1}},
                 {core,{logical,2}},
                 {core,{logical,3}}]}]
    ```

    A more complicated example with two quad core processors, each processor in its own NUMA node. The ordering of logical processors is a bit weird. This to give a better example of identifier lists:

    ```text
    % erl +sct L0-1,3-2c0-3p0N0:L7,4,6-5c0-3p1N1
    1> erlang:system_info(cpu_topology).
    [{node,[{processor,[{core,{logical,0}},
                        {core,{logical,1}},
                        {core,{logical,3}},
                        {core,{logical,2}}]}]},
     {node,[{processor,[{core,{logical,7}},
                        {core,{logical,4}},
                        {core,{logical,6}},
                        {core,{logical,5}}]}]}]
    ```

    As long as real identifiers are correct, it is OK to pass a CPU topology that is not a correct description of the CPU topology. When used with care this can be very useful. This to trick the emulator to bind its schedulers as you want. For example, if you want to run multiple Erlang runtime systems on the same machine, you want to reduce the number of schedulers used and manipulate the CPU topology so that they bind to different logical CPUs. An example, with two Erlang runtime systems on a quad core machine:

    ```text
    % erl +sct L0-3c0-3 +sbt db +S3:2 -detached -noinput -noshell -sname one
    % erl +sct L3-0c0-3 +sbt db +S3:2 -detached -noinput -noshell -sname two
    ```

    In this example, each runtime system have two schedulers each online, and all schedulers online will run on different cores. If we change to one scheduler online on one runtime system, and three schedulers online on the other, all schedulers online will still run on different cores.

    Notice that a faked CPU topology that does not reflect how the real CPU topology looks like is likely to decrease the performance of the runtime system.

    For more information, see [`erlang:system_info(cpu_topology)`](`m:erlang#system_info_cpu_topology`).

  * __`+ssrct`{: id=+ssrct }__ - Skips reading CPU topology.

    > #### Note {: class=info }
    > Reading CPU topology slows down startup when starting many parallel instances of ERTS on systems with large amount of cores, using this flag might speed up execution in such scenarios.

  * __`+sfwi Interval`{: id=+sfwi }__ - Sets scheduler-forced wakeup interval. All run queues are scanned each `Interval` milliseconds. While there are sleeping schedulers in the system, one scheduler is woken for each non-empty run queue found. `Interval` default to `0`, meaning this feature is disabled.

    > #### Note {: class=info }
    > This feature has been introduced as a temporary workaround for long-executing native code, and native code that does not bump reductions properly in OTP. When these bugs have been fixed, this flag will be removed.

  * __`+spp Bool`{: id=+spp }__ - Sets default scheduler hint for port parallelism. If set to `true`, the virtual machine schedules port tasks when it improves parallelism in the system. If set to `false`, the virtual machine tries to perform port tasks immediately, improving latency at the expense of parallelism. Default to `false`. The default used can be inspected in runtime by calling [`erlang:system_info(port_parallelism)`](`m:erlang#system_info_port_parallelism`). The default can be overridden on port creation by passing option [`parallelism`](`m:erlang#open_port_parallelism`) to `erlang:open_port/2`.

  * __`+sss size`{: id=sched_thread_stack_size }__ - Suggested stack size, in kilowords, for scheduler threads. Valid range is 20-8192 kilowords. The default suggested stack size is 128 kilowords.

  * __`+sssdcpu size`{: id=dcpu_sched_thread_stack_size }__ - Suggested stack size, in kilowords, for dirty CPU scheduler threads. Valid range is 20-8192 kilowords. The default suggested stack size is 40 kilowords.

  * __`+sssdio size`{: id=dio_sched_thread_stack_size }__ - Suggested stack size, in kilowords, for dirty IO scheduler threads. Valid range is 20-8192 kilowords. The default suggested stack size is 40 kilowords.

  * __`+stbt BindType`{: id=+stbt }__ - Tries to set the scheduler bind type. The same as flag [`+sbt`](erl_cmd.md#+sbt) except how some errors are handled. For more information, see [`+sbt`](erl_cmd.md#+sbt).

  * __`+sub true|false`{: id=+sub }__ - Enables or disables [scheduler utilization](`m:erlang#statistics_scheduler_wall_time`) balancing of load. By default scheduler utilization balancing is disabled and instead scheduler compaction of load is enabled, which strives for a load distribution that causes as many scheduler threads as possible to be fully loaded (that is, not run out of work). When scheduler utilization balancing is enabled, the system instead tries to balance scheduler utilization between schedulers. That is, strive for equal scheduler utilization on all schedulers.

    `+sub true` is only supported on systems where the runtime system detects and uses a monotonically increasing high-resolution clock. On other systems, the runtime system fails to start.

    `+sub true` implies [`+scl false`](erl_cmd.md#+scl). The difference between `+sub true` and `+scl false` is that `+scl false` does not try to balance the scheduler utilization.

  * __`+swct very_eager|eager|medium|lazy|very_lazy`{: id=+swct }__ - Sets scheduler wake cleanup threshold. Defaults to `medium`. Controls how eager schedulers are to be requesting wakeup because of certain cleanup operations. When a lazy setting is used, more outstanding cleanup operations can be left undone while a scheduler is idling. When an eager setting is used, schedulers are more frequently woken, potentially increasing CPU-utilization.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+sws default|legacy`{: id=+sws }__ - Sets scheduler wakeup strategy. Default strategy changed in ERTS 5.10 (Erlang/OTP R16A). This strategy was known as `proposal` in Erlang/OTP R15. The `legacy` strategy was used as default from R13 up to and including R15.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+swt very_low|low|medium|high|very_high`{: id=+swt }__ - Sets scheduler wakeup threshold. Defaults to `medium`. The threshold determines when to wake up sleeping schedulers when more work than can be handled by currently awake schedulers exists. A low threshold causes earlier wakeups, and a high threshold causes later wakeups. Early wakeups distribute work over multiple schedulers faster, but work does more easily bounce between schedulers.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+swtdcpu very_low|low|medium|high|very_high`{: id=+swtdcpu }__ - As [`+swt`](erl_cmd.md#+swt) but affects dirty CPU schedulers. Defaults to `medium`.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  * __`+swtdio very_low|low|medium|high|very_high`{: id=+swtdio }__ - As [`+swt`](erl_cmd.md#+swt) but affects dirty IO schedulers. Defaults to `medium`.

    > #### Note {: class=info }
    > This flag can be removed or changed at any time without prior notice.

  

* __`+t size`{: id=+t }__ - Sets the maximum number of atoms the virtual machine can handle. Defaults to 1,048,576.

* __`+T Level`{: id=+T }__ - Enables modified timing and sets the modified timing level. Valid range is 0-9. The timing of the runtime system is changed. A high level usually means a greater change than a low level. Changing the timing can be very useful for finding timing-related bugs.

  Modified timing affects the following:

  * __Process spawning__ - A process calling `spawn`, `spawn_link`, `spawn_monitor`, or `spawn_opt` is scheduled out immediately after completing the call. When higher modified timing levels are used, the caller also sleeps for a while after it is scheduled out.

  * __Context reductions__ - The number of reductions a process is allowed to use before it is scheduled out is increased or reduced.

  * __Input reductions__ - The number of reductions performed before checking I/O is increased or reduced.

  > #### Note {: class=info }
  > Performance suffers when modified timing is enabled. This flag is *only* intended for testing and debugging.
  >
  > `return_to` and `return_from` trace messages are lost when tracing on the spawn BIFs.
  >
  > This flag can be removed or changed at any time without prior notice.

* __`+v`__ - Verbose.

* __`+V`__ - Makes the emulator print its version number.

* __`+W w | i | e`__ - Sets the mapping of warning messages for `error_logger`. Messages sent to the error logger using one of the warning routines can be mapped to errors (`+W e`), warnings (`+W w`), or information reports (`+W i`). Defaults to warnings. The current mapping can be retrieved using `error_logger:warning_map/0`. For more information, see `error_logger:warning_map/0` in Kernel.

* __`+zFlag Value`__ - Miscellaneous flags:

  * __`+zdbbl size`{: id=+zdbbl }__ - Sets the distribution buffer busy limit ([`dist_buf_busy_limit`](`m:erlang#system_info_dist_buf_busy_limit`)) in kilobytes. Valid range is 1-2097151. Defaults to 1024.

    A larger buffer limit allows processes to buffer more outgoing messages over the distribution. When the buffer limit has been reached, sending processes will be suspended until the buffer size has shrunk. The buffer limit is per distribution channel. A higher limit gives lower latency and higher throughput at the expense of higher memory use.

    This limit only affects processes that have disabled [*fully asynchronous distributed signaling*](`m:erlang#process_flag_async_dist`).

  * __`+zdntgc time`{: id=+zdntgc }__ - Sets the delayed node table garbage collection time ([`delayed_node_table_gc`](`m:erlang#system_info_delayed_node_table_gc`)) in seconds. Valid values are either `infinity` or an integer in the range 0-100000000. Defaults to 60.

    Node table entries that are not referred linger in the table for at least the amount of time that this parameter determines. The lingering prevents repeated deletions and insertions in the tables from occurring.

  * __`+zosrl limit`{: id=+zosrl }__ - Sets a limit on the amount of outstanding requests made by a system process orchestrating system wide changes. Valid range of this limit is `[1, 134217727]`. See [`erlang:system_flag(outstanding_system_requests_limit, Limit)`](`m:erlang#system_flag_outstanding_system_requests_limit`) for more information.

  

[](){: id=environment_variables }
## Environment Variables

* __`ERL_CRASH_DUMP`__ - If the emulator needs to write a crash dump, the value of this variable is the filename of the crash dump file. If the variable is not set, the name of the crash dump file is `erl_crash.dump` in the current directory.

* __`ERL_CRASH_DUMP_NICE`__ - *Unix systems*: If the emulator needs to write a crash dump, it uses the value of this variable to set the nice value for the process, thus lowering its priority. Valid range is 1-39 (higher values are replaced with 39). The highest value, 39, gives the process the lowest priority.

* __`ERL_CRASH_DUMP_SECONDS`__ - *Unix systems*: This variable gives the number of seconds that the emulator is allowed to spend writing a crash dump. When the given number of seconds have elapsed, the emulator is terminated.

  * __`ERL_CRASH_DUMP_SECONDS=0`__ - If the variable is set to `0` seconds, the runtime system does not even attempt to write the crash dump file. It only terminates. This is the default if option `-heart` is passed to `erl` and `ERL_CRASH_DUMP_SECONDS` is not set.

  * __`ERL_CRASH_DUMP_SECONDS=S`__ - If the variable is set to a positive value `S`, wait for `S` seconds to complete the crash dump file and then terminates the runtime system with a `SIGALRM` signal.

  * __`ERL_CRASH_DUMP_SECONDS=-1`__ - A negative value causes the termination of the runtime system to wait indefinitely until the crash dump file has been completely written. This is the default if option `-heart` is *not* passed to `erl` and `ERL_CRASH_DUMP_SECONDS` is not set.

  See also `m:heart`.

* __`ERL_CRASH_DUMP_BYTES`__ - This variable sets the maximum size of a crash dump file in bytes. The crash dump will be truncated if this limit is exceeded. If the variable is not set, no size limit is enforced by default. If the variable is set to `0`, the runtime system does not even attempt to write a crash dump file.

  Introduced in ERTS 8.1.2 (Erlang/OTP 19.2).

* __`ERL_AFLAGS`{: id=ERL_AFLAGS }__ - The content of this variable is added to the beginning of the command line for `erl`.

  Flag `-extra` is treated in a special way. Its scope ends at the end of the environment variable content. Arguments following an `-extra` flag are moved on the command line into section `-extra`, that is, the end of the command line following an `-extra` flag.

* __`ERL_ZFLAGS`{: id=ERL_ZFLAGS } and `ERL_FLAGS`{: id=ERL_FLAGS }__ - The content of these variables are added to the end of the command line for `erl`.

  Flag `-extra` is treated in a special way. Its scope ends at the end of the environment variable content. Arguments following an `-extra` flag are moved on the command line into section `-extra`, that is, the end of the command line following an `-extra` flag.

* __`ERL_LIBS`__ - Contains a list of additional library directories that the code server searches for applications and adds to the code path; see `m:code`.

* __`ERL_EPMD_ADDRESS`__ - Can be set to a comma-separated list of IP addresses, in which case the [`epmd`](epmd_cmd.md) daemon listens only on the specified address(es) and on the loopback address (which is implicitly added to the list if it has not been specified).

* __`ERL_EPMD_PORT`__ - Can contain the port number to use when communicating with [`epmd`](epmd_cmd.md). The default port works fine in most cases. A different port can be specified to allow nodes of independent clusters to co-exist on the same host. All nodes in a cluster must use the same `epmd` port number.

[](){: id=signals }
## Signals

On Unix systems, the Erlang runtime will interpret two types of signals.

* __`SIGUSR1`__ - A `SIGUSR1` signal forces a crash dump.

* __`SIGTERM`__ - A `SIGTERM` will produce a `stop` message to the `init` process. This is equivalent to a `init:stop/0` call.

  Introduced in ERTS 8.3 (Erlang/OTP 19.3)

The signal `SIGUSR2` is reserved for internal usage. No other signals are handled.

[](){: id=configuration }
## Configuration

The standard Erlang/OTP system can be reconfigured to change the default behavior on startup.

* __The `.erlang` startup file__ - When Erlang/OTP is started, the system searches for a file named `.erlang` in the [user's home directory](`m:init#home`) and then [`filename:basedir(user_config, "erlang")`](`m:filename#user_config`).

  If an `.erlang` file is found, it is assumed to contain valid Erlang expressions. These expressions are evaluated as if they were input to the shell.

  A typical `.erlang` file contains a set of search paths, for example:

  ```text
  io:format("executing user profile in $HOME/.erlang\n",[]).
  code:add_path("/home/calvin/test/ebin").
  code:add_path("/home/hobbes/bigappl-1.2/ebin").
  io:format(".erlang rc finished\n",[]).
  ```

* __user_default and shell_default__ - Functions in the shell that are not prefixed by a module name are assumed to be functional objects (funs), built-in functions (BIFs), or belong to the module `user_default` or `shell_default`.

  To include private shell commands, define them in a module `user_default` and add the following argument as the first line in the `.erlang` file:

  ```text
  code:load_abs("..../user_default").
  ```

* __erl__ - If the contents of `.erlang` are changed and a private version of `user_default` is defined, the Erlang/OTP environment can be customized. More powerful changes can be made by supplying command-line arguments in the startup script `erl`. For more information, see `m:init`.

## See Also

[`epmd(1)`](epmd_cmd.md), `m:erl_prim_loader`, [`erts_alloc(3)`](erts_alloc.md), `m:init`, `m:application`, `m:auth`, `m:code`, `m:erl_boot_server`, `m:heart`, `m:net_kernel`, `m:make`
