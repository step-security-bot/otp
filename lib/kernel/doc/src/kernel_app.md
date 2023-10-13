# kernel

The Kernel application.

## Description

The Kernel application has all the code necessary to run the Erlang runtime system: file servers, code servers, and so on.

The Kernel application is the first application started. It is mandatory in the sense that the minimal system based on Erlang/OTP consists of Kernel and STDLIB. Kernel contains the following functional areas:

* Start, stop, supervision, configuration, and distribution of applications
* Code loading
* Logging
* Global name service
* Supervision of Erlang/OTP
* Communication with sockets
* Operating system interface

## Logger Handlers

Two standard logger handlers are defined in the Kernel application. These are described in the [Kernel User's Guide](logger_chapter.md), and in the `m:logger_std_h` and [`logger_disk_log_h(3)` ](`m:logger_disk_log_h`)manual pages.

[](){: id=erl_signal_server }
## OS Signal Event Handler

Asynchronous OS signals may be subscribed to via the Kernel applications event manager (see [OTP Design Principles](`p:system:design_principles.md`) and `m:gen_event`) registered as `erl_signal_server`. A default signal handler is installed which handles the following signals:

* __`sigusr1`__ - The default handler will halt Erlang and produce a crashdump with slogan "Received SIGUSR1". This is equivalent to calling `erlang:halt("Received SIGUSR1")`.

* __`sigquit`__ - The default handler will halt Erlang immediately. This is equivalent to calling `erlang:halt()`.

* __`sigterm`__ - The default handler will terminate Erlang normally. This is equivalent to calling `init:stop()`.

### Events

Any event handler added to `erl_signal_server` must handle the following events.

* __`sighup`__ - Hangup detected on controlling terminal or death of controlling process

* __`sigquit`__ - Quit from keyboard

* __`sigabrt`__ - Abort signal from abort

* __`sigalrm`__ - Timer signal from alarm

* __`sigterm`__ - Termination signal

* __`sigusr1`__ - User-defined signal 1

* __`sigusr2`__ - User-defined signal 2

* __`sigchld`__ - Child process stopped or terminated

* __`sigstop`__ - Stop process

* __`sigtstp`__ - Stop typed at terminal

Setting OS signals are described in `os:set_signal/2`.

[](){: id=configuration }
## Configuration

The following configuration parameters are defined for the Kernel application. For more information about configuration parameters, see file [`app(4)`](app.md).

* __`connect_all = true | false`{: id=connect_all }__ - If enabled (`true`), which also is the default, `m:global` will actively connect to all nodes that becomes known to it. Note that you also want to enable [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions) in order for `global` to ensure that a fully connected network is maintained. `prevent_overlapping_partitions` will also prevent inconsistencies in `global`'s name registration and locking.

  The now deprecated command line argument [`-connect_all <boolean>`](`p:erts:erl_cmd.md#connect_all`) has the same effect as the `connect_all` configuration parameter. If this configuration parameter is defined, it will override the command line argument.

* __`distributed = [Distrib]`{: id=distributed }__ - Specifies which applications that are distributed and on which nodes they are allowed to execute. In this parameter:

  * `Distrib = {App,Nodes} | {App,Time,Nodes}`
  * `App = atom()`
  * `Time = integer()>0`
  * `Nodes = [node() | {node(),...,node()}]`

  The parameter is described in `application:load/2`.

* __`dist_auto_connect = Value`{: id=dist_auto_connect }__ - Specifies when nodes are automatically connected. If this parameter is not specified, a node is always automatically connected, for example, when a message is to be sent to that node. `Value` is one of:

  * __`never`__ - Connections are never automatically established, they must be explicitly connected. See `m:net_kernel`.

  * __`once`__ - Connections are established automatically, but only once per node. If a node goes down, it must thereafter be explicitly connected. See `m:net_kernel`.

  

* __`permissions = [Perm]`{: id=permissions }__ - Specifies the default permission for applications when they are started. In this parameter:

  * `Perm = {ApplName,Bool}`
  * `ApplName = atom()`
  * `Bool = boolean()`

  Permissions are described in `application:permit/2`.

* __`logger = [Config]`{: id=logger }__ - Specifies the configuration for [Logger](`m:logger`), except the primary log level, which is specified with [`logger_level`](kernel_app.md#logger_level), and the compatibility with [SASL Error Logging](`p:sasl:error_logging.md`), which is specified with [`logger_sasl_compatible`](kernel_app.md#logger_sasl_compatible).

  The `logger `parameter is described in section [Logging](logger_chapter.md#logger_parameter) in the Kernel User's Guide.

* __`logger_level = Level`{: id=logger_level }__ - Specifies the primary log level for Logger. Log events with the same, or a more severe level, pass through the primary log level check. See section [Logging](logger_chapter.md) in the Kernel User's Guide for more information about Logger and log levels.

  `Level = emergency | alert | critical | error | warning | notice | info | debug | all | none`

  To change the primary log level at runtime, use [`logger:set_primary_config(level, Level)`](`logger:set_primary_config/2`).

  Defaults to `notice`.

* __`logger_metadata = Metadata`{: id=logger_metadata }__ - Specifies primary metadata for log events.

  `Metadata = map()`

  Defaults to `#{}`.

* __`logger_sasl_compatible = true | false`{: id=logger_sasl_compatible }__ - Specifies if Logger behaves backwards compatible with the SASL error logging functionality from releases prior to Erlang/OTP 21.0.

  If this parameter is set to `true`, the default Logger handler does not log any progress-, crash-, or supervisor reports. If the SASL application is then started, it adds a Logger handler named `sasl`, which logs these events according to values of the SASL configuration parameter `sasl_error_logger` and `sasl_errlog_type`.

  See section [Deprecated Error Logger Event Handlers and Configuration](`p:sasl:sasl_app.md#deprecated_error_logger_config`) in the sasl(6) manual page for information about the SASL configuration parameters.

  See section [SASL Error Logging](`p:sasl:error_logging.md`) in the SASL User's Guide, and section [Backwards Compatibility with error_logger](logger_chapter.md#compatibility) in the Kernel User's Guide for information about the SASL error logging functionality, and how Logger can be backwards compatible with this.

  Defaults to `false`.

  > #### Note {: class=info }
  > If this parameter is set to `true`, `sasl_errlog_type` indicates that progress reports shall be logged, and the configured primary log level is `notice` or more severe, then SASL automatically sets the primary log level to `info`. That is, this setting can potentially overwrite the value of the Kernel configuration parameter `logger_level`. This is to allow progress reports, which have log level `info`, to be forwarded to the handlers.

* __`global_groups = [GroupTuple]`{: id=global_groups }__ - Defines global groups, see `m:global_group`. In this parameter:

  * `GroupTuple = {GroupName, [Node]} | {GroupName, PublishType, [Node]}`
  * `GroupName = atom()`
  * `PublishType = normal | hidden`
  * `Node = node()`

* __`inet_default_connect_options = [{Opt, Val}]`{: id=inet_default_connect_options }__ - Specifies default options for `connect` sockets, see `m:inet`.

* __`inet_default_listen_options = [{Opt, Val}]`{: id=inet_default_listen_options }__ - Specifies default options for `listen` (and `accept`) sockets, see `m:inet`.

* __`inet_dist_use_interface = ip_address()`{: id=inet_dist_use_interface }__ - If the host of an Erlang node has many network interfaces, this parameter specifies which one to listen on. For the type definition of `ip_address()`, see `m:inet`.

* __`inet_dist_listen_min = First`{: id=inet_dist_listen }  
  `inet_dist_listen_max = Last`__  
  Defines the `First..Last` port range for the listener socket of a distributed Erlang node.

* __`inet_dist_listen_options = Opts`{: id=inet_dist_listen_options }__ - Defines a list of extra socket options to be used when opening the listening socket for a distributed Erlang node. See `gen_tcp:listen/2`.

* __`inet_dist_connect_options = Opts`{: id=inet_dist_connect_options }__ - Defines a list of extra socket options to be used when connecting to other distributed Erlang nodes. See `gen_tcp:connect/4`.

* __`inet_parse_error_log = silent`{: id=inet_parse_error_log }__ - If set, no log events are issued when erroneous lines are found and skipped in the various Inet configuration files.

* __`inetrc = Filename`{: id=inetrc }__ - The name (string) of an Inet user configuration file. For details, see section [`Inet Configuration`](`p:erts:inet_cfg.md`) in the ERTS User's Guide.

* __`net_setuptime = SetupTime`{: id=net_setuptime }__ - [](){: id=net_setuptime }
  `SetupTime` must be a positive integer or floating point number, and is interpreted as the maximum allowed time for each network operation during connection setup to another Erlang node. The maximum allowed value is `120`. If higher values are specified, `120` is used. Default is 7 seconds if the variable is not specified, or if the value is incorrect (for example, not a number).

  Notice that this value does not limit the total connection setup time, but rather each individual network operation during the connection setup and handshake.

* __`net_ticker_spawn_options = Opts`{: id=net_ticker_spawn_options }__ - Defines a list of extra spawn options for net ticker processes. There exist one such process for each connection to another node. A net ticker process is responsible for supervising the connection it is associated with. These processes also execute the distribution handshake protocol when setting up connections. When there is a large number of distribution connections, setting up garbage collection options can be helpful to reduce memory usage. Default is `[link, {priority, max}]`, and these two options cannot be changed. The `monitor` and `{monitor, MonitorOpts}` options are not allowed and will be dropped if present. See the documentation of the `erlang:spawn_opt/4` BIF for information about valid options. If the `Opts` list is not a proper list, or containing invalid options the setup of connections will fail.

  Note that the behavior described above is only true if the distribution carrier protocol used is implemented as described in [ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](`p:erts:alt_dist.md#distribution_module`) without further alterations. The implementer of the distribution carrier protocol used, may have chosen to ignore the `net_ticker_spawn_options` parameter or altered its behavior. Currently all distribution modules shipped with OTP do, however, behave as described above.

* __`net_tickintensity = NetTickIntensity`{: id=net_tickintensity }__ - *Net tick intensity* specifies how many ticks to send during a [net tick time](kernel_app.md#net_ticktime) period when no other data is sent over a connection to another node. This also determines how often to check for data from the other node. The higher net tick intensity, the closer to the chosen net tick time period the node will detect an unresponsive node. The net tick intensity defaults to `4`. The value of `NetTickIntensity` should be an integer in the range `4..1000`. If the `NetTickIntensity` is not an integer or an integer less than `4`, `4` will silently be used. If `NetTickIntensity` is an integer larger than `1000`, `1000` will silently be used.

  > #### Note {: class=info }
  > Note that all communicating nodes are expected to use the same *net tick intensity* as well as the same *net tick time*.

  > #### Warning {: class=warning }
  > Be careful not to set a too high net tick intensity, since you can overwhelm the node with work if it is set too high.

* __`net_ticktime = NetTickTime`{: id=net_ticktime }__ - Specifies the *net tick time* in seconds. This is the approximate time a connected node may be unresponsive until it is considered down and thereby disconnected.

  Net tick time together with [net tick intensity](kernel_app.md#net_tickintensity) determines an interval `TickInterval = NetTickTime/NetTickIntensity`. Once every `TickInterval` seconds, each connected node is ticked if nothing has been sent to it during that last `TickInterval` seconds. A tick is a small package sent on the connection. A connected node is considered to be down if no ticks or payload packages have been received during the last `NetTickIntensity` number of `TickInterval` seconds intervals. This ensures that nodes that are not responding, for reasons such as hardware errors, are considered to be down.

  As the availability is only checked every `TickInterval` seconds, the actual time `T` a node have been unresponsive when detected may vary between `MinT` and `MaxT`, where:

  ```text
  MinT = NetTickTime - NetTickTime / NetTickIntensity
  MaxT = NetTickTime + NetTickTime / NetTickIntensity
  ```

  `NetTickTime` defaults to `60` seconds and `NetTickIntensity` defaults to `4`. Thus, `45 < T < 75` seconds.

  > #### Note {: class=info }
  > Notice that *all* communicating nodes are to have the *same* `NetTickTime` and `NetTickIntensity` values specified, as it determines both the frequency of outgoing ticks and the expected frequency of incominging ticks.

  `NetTickTime` needs to be a multiple of `NetTickIntensity`. If the configured values are not, `NetTickTime` will internally be rounded up to the nearest millisecond. [`net_kernel:get_net_ticktime()`](`net_kernel:get_net_ticktime/0`) will, however, report net tick time truncated to the nearest second.

  Normally, a terminating node is detected immediately by the transport protocol (like TCP/IP).

* __`prevent_overlapping_partitions = true | false`{: id=prevent_overlapping_partitions }__ - If enabled (`true`), `global` will actively prevent overlapping partitions from forming when connections are lost between nodes. This fix is enabled by default. If you are about to disable this fix, make sure to read the [`global(3)`](`m:global#prevent_overlapping_partitions`) documentation about this fix for more important information about this.

* __`shutdown_timeout = integer() | infinity`{: id=shutdown_timeout }__ - Specifies the time `application_controller` waits for an application to terminate during node shutdown. If the timer expires, `application_controller` brutally kills `application_master` of the hanging application. If this parameter is undefined, it defaults to `infinity`.

* __`sync_nodes_mandatory = [NodeName]`{: id=sync_nodes_mandatory }__ - Specifies which other nodes that *must* be alive for this node to start properly. If some node in the list does not start within the specified time, this node does not start either. If this parameter is undefined, it defaults to `[]`.

* __`sync_nodes_optional = [NodeName]`{: id=sync_nodes_optional }__ - Specifies which other nodes that *can* be alive for this node to start properly. If some node in this list does not start within the specified time, this node starts anyway. If this parameter is undefined, it defaults to the empty list.

* __`sync_nodes_timeout = integer() | infinity`{: id=sync_nodes_timeout }__ - Specifies the time (in milliseconds) that this node waits for the mandatory and optional nodes to start. If this parameter is undefined, no node synchronization is performed. This option ensures that `global` is synchronized.

* __`start_distribution = true | false`{: id=start_distribution }__ - Starts all distribution services, such as `rpc`, `global`, and `net_kernel` if the parameter is `true`. This parameter is to be set to `false` for systems who want to disable all distribution functionality.

  Defaults to `true`.

* __`start_dist_ac = true | false`{: id=start_dist_ac }__ - Starts the `dist_ac` server if the parameter is `true`. This parameter is to be set to `true` for systems using distributed applications.

  Defaults to `false`. If this parameter is undefined, the server is started if parameter `distributed` is set.

* __`start_boot_server = true | false`{: id=start_boot_server }__ - Starts the `boot_server` if the parameter is `true` (see `m:erl_boot_server`). This parameter is to be set to `true` in an embedded system using this service.

  Defaults to `false`.

* __`boot_server_slaves = [SlaveIP]`{: id=boot_server_slaves }__ - If configuration parameter `start_boot_server` is `true`, this parameter can be used to initialize `boot_server` with a list of slave IP addresses:

  `SlaveIP = string() | atom | {integer(),integer(),integer(),integer()}`,

  where `0 <= integer() <=255`.

  Examples of `SlaveIP` in atom, string, and tuple form:

  `'150.236.16.70', "150,236,16,70", {150,236,16,70}`.

  Defaults to `[]`.

* __`start_disk_log = true | false`{: id=start_disk_log }__ - Starts the `disk_log_server` if the parameter is `true` (see `m:disk_log`). This parameter is to be set to `true` in an embedded system using this service.

  Defaults to `false`.

* __`start_pg = true | false`{: id=start_pg }__ - [](){: id=start_pg }
  Starts the default `pg` scope server (see `m:pg`) if the parameter is `true`. This parameter is to be set to `true` in an embedded system that uses this service.

  Defaults to `false`.

* __`start_timer = true | false`{: id=start_timer }__ - Starts the `timer_server` if the parameter is `true` (see `m:timer`). This parameter is to be set to `true` in an embedded system using this service.

  Defaults to `false`.

* __`shell_history = enabled | disabled | module()`{: id=shell_history }__ - Specifies whether shell history should be logged to disk between usages of `erl` (`enabled`), not logged at all (`disabled`), or a user-specified module will be used to log shell history. This module should export `load() -> [string()]` returning a list of strings to load in the shell when it starts, and `add(iodata()) -> ok.` called every time new line is entered in the shell. By default logging is disabled.

* __`shell_history_drop = [string()]`{: id=shell_history_drop }__ - Specific log lines that should not be persisted. For example `["q().", "init:stop()."]` will allow to ignore commands that shut the node down. Defaults to `[]`.

* __`shell_history_file_bytes = integer()`{: id=shell_history_file_bytes }__ - How many bytes the shell should remember. By default, the value is set to 512kb, and the minimal value is 50kb.

* __`shell_history_path = string()`{: id=shell_history_path }__ - Specifies where the shell history files will be stored. defaults to the user's cache directory as returned by `filename:basedir(user_cache, "erlang-history")`.

* __`shutdown_func = {Mod, Func}`{: id=shutdown_func }__ - Where:

  * `Mod = atom()`
  * `Func = atom()`

  Sets a function that `application_controller` calls when it starts to terminate. The function is called as `Mod:Func(Reason)`, where `Reason` is the terminate reason for `application_controller`, and it must return as soon as possible for `application_controller` to terminate properly.

* __`source_search_rules = [DirRule] | [SuffixRule]`{: id=source_search_rules }__ - [](){: id=source_search_rules }
  Where:

  * `DirRule = {ObjDirSuffix,SrcDirSuffix}`
  * `SuffixRule = {ObjSuffix,SrcSuffix,[DirRule]}`
  * `ObjDirSuffix = string()`
  * `SrcDirSuffix = string()`
  * `ObjSuffix = string()`
  * `SrcSuffix = string()`

  Specifies a list of rules for use by `filelib:find_file/2` `filelib:find_source/2` If this is set to some other value than the empty list, it replaces the default rules. Rules can be simple pairs of directory suffixes, such as `{"ebin", "src"}`, which are used by `filelib:find_file/2`, or triples specifying separate directory suffix rules depending on file name extensions, for example `[{".beam", ".erl", [{"ebin", "src"}]}`, which are used by `filelib:find_source/2`. Both kinds of rules can be mixed in the list.

  The interpretation of `ObjDirSuffix` and `SrcDirSuffix` is as follows: if the end of the directory name where an object is located matches `ObjDirSuffix`, then the name created by replacing `ObjDirSuffix` with `SrcDirSuffix` is expanded by calling `filelib:wildcard/1`, and the first regular file found among the matches is the source file.

* __`standard_io_encoding = Encoding`{: id=standard_io_encoding }__ - Set whether bytes sent or received via standard_io should be interpreted as unicode or latin1. By default input and output is interpreted as Unicode if it is supported on the host. With this flag you may configure the encoding on startup.

  This works similarly to [`io:setopts(standard_io, {encoding, Encoding})`](`io:setopts/2`) but is applied before any bytes on standard_io may have been read.

  Encoding is one of:

  * __`unicode`__ - Configure standard_io to use unicode mode.

  * __`latin1`__ - Configure standard_io to use latin1 mode.

  * __`_`__ - Anything other than unicode or latin1 will be ignored and the system will configure the encoding by itself, typically unicode on modern systems.

  See [Escripts and non-interactive I/O in Unicode Usage in Erlang](`p:stdlib:unicode_usage.md#escripts-and-non-interactive-i-o`) for more details.

## Deprecated Configuration Parameters

In Erlang/OTP 21.0, a new API for logging was added. The old `error_logger` event manager, and event handlers running on this manager, still work, but they are no longer used by default.

The following application configuration parameters can still be set, but they are only used if the corresponding configuration parameters for Logger are not set.

* __`error_logger`__ - Replaced by setting the [`type`](`m:logger_std_h#type`), and possibly [`file`](`m:logger_std_h#file`) and [`modes`](`m:logger_std_h#modes`) parameters of the default `logger_std_h` handler. Example:

  ```text
  erl -kernel logger '[{handler,default,logger_std_h,#{config=>#{file=>"/tmp/erlang.log"}}}]'
  ```

* __`error_logger_format_depth`__ - Replaced by setting the [`depth`](`m:logger_formatter#depth`) parameter of the default handlers formatter. Example:

  ```text
  erl -kernel logger '[{handler,default,logger_std_h,#{formatter=>{logger_formatter,#{legacy_header=>true,template=>[{logger_formatter,header},"\n",msg,"\n"],depth=>10}}}]'
  ```

See [Backwards compatibility with error_logger](logger_chapter.md#compatibility) for more information.

## See Also

[`app(4)`](app.md), `m:application`, `m:code`, `m:disk_log`, `m:erl_boot_server`, `m:erl_ddll`, `m:file`, `m:global`, `m:global_group`, `m:heart`, `m:inet`, `m:logger`, `m:net_kernel`, `m:os`, `m:pg`, `m:rpc`, `m:seq_trace`, `m:timer`
