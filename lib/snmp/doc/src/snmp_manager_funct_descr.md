# Manager Functional Description

## Features

[](){: id=features }
The manager provided with the tool is a lightweight manager that basically provides a means to communicate with agents.

It does not really implement any management capabilities by itself. That is up to the *user*.

A *user* in this context is basically a module implementing the `m:snmpm_user` behaviour. A *user* can issue snmp requests and receive notification/traps.

Agents to be accessed by the manager needs to be registered by a user. Once registered, they can be accessed by all registered users.

Notifications/traps from an agent is delivered to the user that did the registration.

Any message from an agent that is not registered is delivered to the *default user*.

By default, the *default user* is set to the `snmpm_user_default` module, which simply sends an info message to the error_logger. It is however highly recommended that this module be replaced by another that does something useful (see [configuration params](snmp_config.md#configuration_params) for more info).

When using version 3, then (at least one) *usm user* has to be registered.

Requests can be issued in two different ways. Synchronous (see [sync_set](`m:snmpm#sync_set2`), [sync_get](`m:snmpm#sync_get2`), [sync_get_next](`m:snmpm#sync_get_next2`) and [sync_get_bulk](`m:snmpm#sync_get_bulk2`)) and asynchronous (see [async_set](`m:snmpm#async_set2`), [async_get](`m:snmpm#async_get2`), [async_get_next](`m:snmpm#async_get_next2`) and [async_get_bulk](`m:snmpm#async_get_bulk2`)). With synchronous the snmp reply is returned by the function. With asynchronous, the reply will instead be delivered through a call to one of the `handle_pdu` callback function defined by the [handle_pdu](`m:snmpm_user#handle_pdu`) behaviour.

## Operation

[](){: id=operation }
The following steps are needed to get the manager running:

1. \[optional] Implement the default user.
1. Implement the user(s).
1. Configure the application (manager).
1. Start the application (manager).
1. Register the user(s).
1. The user(s) register their agents.

## MIB loading

[](){: id=mib_loading }
It is possible to load mibs into the manager, but this is not necessary for normal operation, and not recommended.
