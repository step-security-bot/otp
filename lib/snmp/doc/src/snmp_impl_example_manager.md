# Manager Implementation Example

This *Implementation Example* section describes how a simple manager can be implemented with the SNMP Development Toolkit.

The example shown, *ex2*, can be found in the toolkit distribution.

This example has two functions:

* A simple example of how to use the manager component of the SNMP Development Toolkit.
* A simple example of how to write agent test cases, using the new manager.

## The example manager

The example manager, `snmp_ex2_manager`, is a simple example of how to implement an snmp manager using the manager component of the SNMP Development Toolkit.

The module exports the following functions:

* start_link/0, start_link/1
* stop/0
* agent/2, agent/3
* sync_get/2, sync_get/3
* sync_get_next/2, sync_get_next/3
* sync_get_bulk/4, sync_get_bulk/5
* sync_set/2, sync_set/3
* oid_to_name/1

This module is also used by the test module described in the next section.

## A simple standard test

This simple standard test, `snmp_ex2_simple_standard_test`, a module which, using the `snmp_ex2_manager` described in the previous section, implements a simple agent test utility.
