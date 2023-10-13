# os_mon

OS Monitoring Application

## Description

The operating system monitor, OS_Mon, provides the following services:

* `m:cpu_sup` CPU load and utilization supervision (Unix)
* `m:disksup` Disk supervision(Unix, Windows)
* `m:memsup` Memory supervision (Unix, Windows)
* `m:os_sup` Interface to OS system messages (Solaris, Windows)

To simplify usage of OS_Mon on distributed Erlang systems, it is not considered an error trying to use a service at a node where it is not available (either because OS_Mon is not running, or because the service is not available for that OS, or because the service is not started). Instead, a warning message is issued via `error_logger` and a dummy value is returned, which one is specified in the man pages for the respective services.

## Configuration

When OS_Mon is started, by default all services available for the OS, except `os_sup`, are automatically started. This configuration can be changed using the following application configuration parameters:

* __`start_cpu_sup = bool()`__ - Specifies if `cpu_sup` should be started. Defaults to `true`.

* __`start_disksup = bool()`__ - Specifies if `disksup` should be started. Defaults to `true`.

* __`start_memsup = bool()`__ - Specifies if `memsup` should be started. Defaults to `true`.

* __`start_os_sup = bool()`__ - Specifies if `os_sup` should be started. Defaults to `false`.

Configuration parameters effecting the different OS_Mon services are described in the respective man pages.

See [config(4)](`p:kernel:config.md`) for information about how to change the value of configuration parameters.

## See Also

`m:cpu_sup`, `m:disksup`, `m:memsup`, `m:os_sup`, `m:nteventlog`, `m:snmp`.
