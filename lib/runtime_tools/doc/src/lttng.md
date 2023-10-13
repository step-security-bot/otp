# LTTng and Erlang/OTP

## Introduction

The Linux Trace Toolkit: next generation is an open source system software package for correlated tracing of the Linux kernel, user applications and libraries.

For more information, please visit [http://lttng.org](http://lttng.org)

## Building Erlang/OTP with LTTng support

Configure and build Erlang with LTTng support:

For LTTng to work properly with Erlang/OTP you need the following packages installed:

* LTTng-tools: a command line interface to control tracing sessions.
* LTTng-UST: user space tracing library.

On Ubuntu this can be installed via `aptitude`:

```text
$ sudo aptitude install lttng-tools liblttng-ust-dev
```

See [Installing LTTng](http://lttng.org/docs/#doc-installing-lttng) for more information on how to install LTTng on your system.

After LTTng is properly installed on the system Erlang/OTP can be built with LTTng support.

```text
$ ./configure --with-dynamic-trace=lttng
$ make
```

## Dyntrace Tracepoints

All tracepoints are in the domain of `org_erlang_dyntrace`

All Erlang types are the string equivalent in LTTng.

*process_spawn*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `parent : string` :: Process ID. Ex. `"<0.131.0>"`
* `entry : string` :: Code Location. Ex. `"lists:sort/1"`

Available through `erlang:trace/3` with trace flag `procs` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_spawn: { cpu_id = 3 }, { pid = "<0.131.0>", parent = "<0.130.0>", entry = "erlang:apply/2" }
```

*process_link*

* `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
* `from : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
* `type : string` :: `"link" | "unlink"`

Available through `erlang:trace/3` with trace flag `procs` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_link: { cpu_id = 3 }, { from = "<0.130.0>", to = "<0.131.0>", type = "link" }
```

*process_exit*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `reason : string` :: Exit reason. Ex. `"normal"`

Available through `erlang:trace/3` with trace flag `procs` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_exit: { cpu_id = 3 }, { pid = "<0.130.0>", reason = "normal" }
```

*process_register*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `name : string` :: Registered name. Ex. `"logger"`
* `type : string` :: `"register" | "unregister"`

Example:

```text
process_register: { cpu_id = 0 }, { pid = "<0.128.0>", name = "dyntrace_lttng_SUITE" type = "register" }
```

*process_scheduled*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `entry : string` :: Code Location. Ex. `"lists:sort/1"`
* `type : string` :: `"in" | "out" | "in_exiting" | "out_exiting" | "out_exited"`

Available through `erlang:trace/3` with trace flag `running` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_scheduled: { cpu_id = 0 }, { pid = "<0.136.0>", entry = "erlang:apply/2", type = "in" }
```

*port_open*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`

Available through `erlang:trace/3` with trace flag `ports` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
port_open: { cpu_id = 5 }, { pid = "<0.131.0>", driver = "'/bin/sh -s unix:cmd'", port = "#Port<0.1887>" }
```

*port_exit*

* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `reason : string` :: Exit reason. Ex. `"normal"`

Available through `erlang:trace/3` with trace flag `ports` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
port_exit: { cpu_id = 5 }, { port = "#Port<0.1887>", reason = "normal" }
```

*port_link*

* `to : string` :: Process ID. Ex. `"<0.131.0>"`
* `from : string` :: Process ID. Ex. `"<0.131.0>"`
* `type : string` :: `"link" | "unlink"`

Available through `erlang:trace/3` with trace flag `ports` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
port_link: { cpu_id = 5 }, { from = "#Port<0.1887>", to = "<0.131.0>", type = "unlink" }
```

*port_scheduled*

Available through `erlang:trace/3` with trace flag `running` and `{tracer,dyntrace,[]}` as tracer module.

* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `entry : string` :: Callback. Ex. `"open"`
* `type : string` :: `"in" | "out" | "in_exiting" | "out_exiting" | "out_exited"`

Example:

```text
port_scheduled: { cpu_id = 5 }, { pid = "#Port<0.1905>", entry = "close", type = "out" }
```

Available through `erlang:trace/3` with trace flag `running` and `{tracer,dyntrace,[]}` as tracer module.

*function_call*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `entry : string` :: Code Location. Ex. `"lists:sort/1"`
* `depth : integer` :: Stack depth. Ex. `0`

Available through `erlang:trace/3` with trace flag `call` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
function_call: { cpu_id = 5 }, { pid = "<0.145.0>", entry = "dyntrace_lttng_SUITE:'-t_call/1-fun-1-'/0", depth = 0 }
```

*function_return*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `entry : string` :: Code Location. Ex. `"lists:sort/1"`
* `depth : integer` :: Stack depth. Ex. `0`

Available through `erlang:trace/3` with trace flag `call` or `return_to` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
function_return: { cpu_id = 5 }, { pid = "<0.145.0>", entry = "dyntrace_lttng_SUITE:waiter/0", depth = 0 }
```

*function_exception*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `entry : string` :: Code Location. Ex. `"lists:sort/1"`
* `class : string` :: Error reason. Ex. `"error"`

Available through `erlang:trace/3` with trace flag `call` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
function_exception: { cpu_id = 5 }, { pid = "<0.144.0>", entry = "t:call_exc/1", class = "error" }
```

*message_send*

* `from : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
* `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
* `message : string` :: Message sent. Ex. `"{<0.162.0>,ok}"`

Available through `erlang:trace/3` with trace flag `send` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
message_send: { cpu_id = 3 }, { from = "#Port<0.1938>", to = "<0.160.0>", message = "{#Port<0.1938>,eof}" }
```

*message_receive*

* `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
* `message : string` :: Message received. Ex. `"{<0.162.0>,ok}"`

Available through `erlang:trace/3` with trace flag `'receive'` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
message_receive: { cpu_id = 7 }, { to = "<0.167.0>", message = "{<0.165.0>,ok}" }
```

*gc_minor_start*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `need : integer` :: Heap need. Ex. `2`
* `heap : integer` :: Young heap word size. Ex. `233`
* `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
gc_minor_start: { cpu_id = 0 }, { pid = "<0.172.0>", need = 0, heap = 610, old_heap = 0 }
```

*gc_minor_end*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `reclaimed : integer` :: Heap reclaimed. Ex. `2`
* `heap : integer` :: Young heap word size. Ex. `233`
* `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
gc_minor_end: { cpu_id = 0 }, { pid = "<0.172.0>", reclaimed = 120, heap = 1598, old_heap = 1598 }
```

*gc_major_start*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `need : integer` :: Heap need. Ex. `2`
* `heap : integer` :: Young heap word size. Ex. `233`
* `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
gc_major_start: { cpu_id = 0 }, { pid = "<0.172.0>", need = 8, heap = 2586, old_heap = 1598 }
```

*gc_major_end*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `reclaimed : integer` :: Heap reclaimed. Ex. `2`
* `heap : integer` :: Young heap word size. Ex. `233`
* `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and `{tracer,dyntrace,[]}` as tracer module.

Example:

```text
gc_major_end: { cpu_id = 0 }, { pid = "<0.172.0>", reclaimed = 240, heap = 4185, old_heap = 0 }
```

## BEAM Tracepoints

All tracepoints are in the domain of `org_erlang_otp`

All Erlang types are the string equivalent in LTTng.

*driver_init*

* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `major : integer` :: Major version. Ex. `3`
* `minor : integer` :: Minor version. Ex. `1`
* `flags : integer` :: Flags. Ex. `1`

Example:

```text
driver_init: { cpu_id = 2 }, { driver = "caller_drv", major = 3, minor = 3, flags = 1 }
```

*driver_start*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`

Example:

```text
driver_start: { cpu_id = 2 }, { pid = "<0.198.0>", driver = "caller_drv", port = "#Port<0.3676>" }
```

*driver_output*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```text
driver_output: { cpu_id = 2 }, { pid = "<0.198.0>", port = "#Port<0.3677>", driver = "/bin/sh -s unix:cmd", bytes = 36 }
```

*driver_outputv*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```text
driver_outputv: { cpu_id = 5 }, { pid = "<0.194.0>", port = "#Port<0.3663>", driver = "tcp_inet", bytes = 3 }
```

*driver_ready_input*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_ready_input: { cpu_id = 5 }, { pid = "<0.189.0>", port = "#Port<0.3637>", driver = "inet_gethost 4 " }
```

*driver_ready_output*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_ready_output: { cpu_id = 5 }, { pid = "<0.194.0>", port = "#Port<0.3663>", driver = "tcp_inet" }
```

*driver_timeout*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_timeout: { cpu_id = 5 }, { pid = "<0.196.0>", port = "#Port<0.3664>", driver = "tcp_inet" }
```

*driver_stop_select*

* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_stop_select: { cpu_id = 5 }, { driver = "unknown" }
```

*driver_flush*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_flush: { cpu_id = 7 }, { pid = "<0.204.0>", port = "#Port<0.3686>", driver = "tcp_inet" }
```

*driver_stop*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_stop: { cpu_id = 5 }, { pid = "[]", port = "#Port<0.3673>", driver = "tcp_inet" }
```

*driver_process_exit*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

*driver_ready_async*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_ready_async: { cpu_id = 3 }, { pid = "<0.181.0>", port = "#Port<0.3622>", driver = "tcp_inet" }
```

*driver_call*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `command : integer` :: Command integer. Ex. `1`
* `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```text
driver_call: { cpu_id = 2 }, { pid = "<0.202.0>", port = "#Port<0.3676>", driver = "caller_drv", command = 0, bytes = 2 }
```

*driver_control*

* `pid : string` :: Process ID. Ex. `"<0.131.0>"`
* `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
* `driver : string` :: Driver name. Ex. `"tcp_inet"`
* `command : integer` :: Command integer. Ex. `1`
* `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```text
driver_control: { cpu_id = 3 }, { pid = "<0.32767.8191>", port = "#Port<0.0>", driver = "forker", command = 83, bytes = 32 }
```

*carrier_create*

* `type : string` :: Carrier type. Ex. `"ets_alloc"`
* `instance : integer` :: Allocator instance. Ex. `1`
* `size : integer` :: Carrier size. Ex. `262144`
* `mbc_carriers : integer` :: Number of multiblock carriers in instance. Ex. `3`
* `mbc_carriers_size : integer` :: Total size of multiblock blocks carriers in instance. Ex. `1343488`
* `mbc_blocks : integer` :: Number of multiblock blocks in instance. Ex. `122`
* `mbc_blocks_size : integer` :: Total size of all multiblock blocks in instance. Ex. `285296`
* `sbc_carriers : integer` :: Number of singleblock carriers in instance. Ex. `1`
* `sbc_carriers_size : integer` :: Total size of singleblock blocks carriers in instance. Ex. `1343488`
* `sbc_blocks : integer` :: Number of singleblocks in instance. Ex. `1`
* `sbc_blocks_size : integer` :: Total size of all singleblock blocks in instance. Ex. `285296`

Example:

```text
carrier_create: { cpu_id = 2 }, { type = "ets_alloc", instance = 7, size = 2097152, mbc_carriers = 4, mbc_carriers_size = 3440640, mbc_blocks = 526, mbc_blocks_size = 1278576, sbc_carriers = 0, sbc_carriers_size = 0, sbc_blocks = 0, sbc_blocks_size = 0 }
```

*carrier_destroy*

* `type : string` :: Carrier type. Ex. `"ets_alloc"`
* `instance : integer` :: Allocator instance. Ex. `1`
* `size : integer` :: Carrier size. Ex. `262144`
* `mbc_carriers : integer` :: Number of multiblock carriers in instance. Ex. `3`
* `mbc_carriers_size : integer` :: Total size of multiblock blocks carriers in instance. Ex. `1343488`
* `mbc_blocks : integer` :: Number of multiblock blocks in instance. Ex. `122`
* `mbc_blocks_size : integer` :: Total size of all multiblock blocks in instance. Ex. `285296`
* `sbc_carriers : integer` :: Number of singleblock carriers in instance. Ex. `1`
* `sbc_carriers_size : integer` :: Total size of singleblock blocks carriers in instance. Ex. `1343488`
* `sbc_blocks : integer` :: Number of singleblocks in instance. Ex. `1`
* `sbc_blocks_size : integer` :: Total size of all singleblock blocks in instance. Ex. `285296`

Example:

```text
carrier_destroy: { cpu_id = 6 }, { type = "ets_alloc", instance = 7, size = 262144, mbc_carriers = 3, mbc_carriers_size = 3178496, mbc_blocks = 925, mbc_blocks_size = 2305336, sbc_carriers = 0, sbc_carriers_size = 0, sbc_blocks = 0, sbc_blocks_size = 0 }
```

*carrier_pool_put*

* `type : string` :: Carrier type. Ex. `"ets_alloc"`
* `instance : integer` :: Allocator instance. Ex. `1`
* `size : integer` :: Carrier size. Ex. `262144`

Example:

```text
carrier_pool_put: { cpu_id = 3 }, { type = "ets_alloc", instance = 5, size = 1048576 }
```

*carrier_pool_get*

* `type : string` :: Carrier type. Ex. `"ets_alloc"`
* `instance : integer` :: Allocator instance. Ex. `1`
* `size : integer` :: Carrier size. Ex. `262144`

Example:

```text
carrier_pool_get: { cpu_id = 7 }, { type = "ets_alloc", instance = 4, size = 3208 }
```

## Example of process tracing

An example of process tracing of `os_mon` and friends.

Clean start of lttng in a bash shell.

```text
$ lttng create erlang-demo
Spawning a session daemon
Session erlang-demo created.
Traces will be written in /home/egil/lttng-traces/erlang-demo-20160526-165920
```

Start an Erlang node with lttng enabled.

```text
$ erl
Erlang/OTP 19 [erts-8.0] [source-4d7b24d] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [lttng]

Eshell V8.0  (abort with ^G)
1>
```

Load the `dyntrace` module.

```text
1> l(dyntrace).
{module,dyntrace}
```

All tracepoints via dyntrace are now visible and can be listed through `lttng list -u`.

Enable the process_register LTTng tracepoint for Erlang.

```text
$ lttng enable-event -u org_erlang_dyntrace:process_register
UST event org_erlang_dyntrace:process_register created in channel channel0
```

Enable process tracing for new processes and use `dyntrace` as tracer backend.

```text
2> erlang:trace(new,true,[procs,{tracer,dyntrace,[]}]).
0
```

Start LTTng tracing.

```text
$ lttng start
Tracing started for session erlang-demo
```

Start the `os_mon` application in Erlang.

```text
3> application:ensure_all_started(os_mon).
{ok,[sasl,os_mon]}
```

Stop LTTng tracing and view the result.

```text
$ lttng stop
Tracing stopped for session erlang-demo
$ lttng view
[17:20:42.561168759] (+?.?????????) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.66.0>", name = "sasl_sup", type = "register" }
[17:20:42.561215519] (+0.000046760) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.67.0>", name = "sasl_safe_sup", type = "register" }
[17:20:42.562149024] (+0.000933505) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.68.0>", name = "alarm_handler", type = "register" }
[17:20:42.571035803] (+0.008886779) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.69.0>", name = "release_handler", type = "register" }
[17:20:42.574939868] (+0.003904065) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.74.0>", name = "os_mon_sup", type = "register" }
[17:20:42.576818712] (+0.001878844) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.75.0>", name = "disksup", type = "register" }
[17:20:42.580032013] (+0.003213301) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.76.0>", name = "memsup", type = "register" }
[17:20:42.583046339] (+0.003014326) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.78.0>", name = "cpu_sup", type = "register" }
[17:20:42.586206242] (+0.003159903) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.82.0>", name = "timer_server", type = "register" }
```
