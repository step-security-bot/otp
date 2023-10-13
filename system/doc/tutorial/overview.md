# Overview

## Built-In Mechanisms

Two interoperability mechanisms are built into the Erlang runtime system, *distributed Erlang* and *ports*. A variation of ports is *linked-in drivers*.

[](){: id=dist }
### Distributed Erlang

An Erlang runtime system is made a distributed Erlang node by giving it a name. A distributed Erlang node can connect to, and monitor, other nodes. It can also spawn processes at other nodes. Message passing and error handling between processes at different nodes are transparent. A number of useful STDLIB modules are available in a distributed Erlang system. For example, `global`, which provides global name registration. The distribution mechanism is implemented using TCP/IP sockets.

*When to use:* Distributed Erlang is primarily used for Erlang-Erlang communication. It can also be used for communication between Erlang and C, if the C program is implemented as a C node, see [C and Java Libraries](overview.md#cnode).

*Where to read more:* Distributed Erlang and some distributed programming techniques are described in the Erlang book.

For more information, see [Distributed Programming.](`p:system:conc_prog.md#distributed programming`)

Relevant manual pages are the following:

* `m:erlang` manual page in ERTS (describes the BIFs)
* `m:global` manual page in Kernel
* `m:net_adm` manual page in Kernel
* `m:pg` manual page in Kernel
* `m:rpc` manual page in Kernel
* `m:pool` manual page in STDLIB
* `m:slave` manual page in STDLIB

### Ports and Linked-In Drivers

Ports provide the basic mechanism for communication with the external world, from Erlang's point of view. The ports provide a byte-oriented interface to an external program. When a port is created, Erlang can communicate with it by sending and receiving lists of bytes (not Erlang terms). This means that the programmer might have to invent a suitable encoding and decoding scheme.

The implementation of the port mechanism depends on the platform. For UNIX, pipes are used and the external program is assumed to read from standard input and write to standard output. The external program can be written in any programming language as long as it can handle the interprocess communication mechanism with which the port is implemented.

The external program resides in another OS process than the Erlang runtime system. In some cases this is not acceptable. Consider, for example, drivers with very hard time requirements. It is therefore possible to write a program in C according to certain principles, and dynamically link it to the Erlang runtime system. This is called a *linked-in driver*.

*When to use:* Ports can be used for all kinds of interoperability situations where the Erlang program and the other program runs on the same machine. Programming is fairly straight-forward.

Linked-in drivers involves writing certain call-back functions in C. This requires very good skills as the code is linked to the Erlang runtime system.

> #### Warning {: class=warning }
> A faulty linked-in driver causes the entire Erlang runtime system to leak memory, hang, or crash.

*Where to read more:* Ports are described in section "Miscellaneous Items" of the Erlang book. Linked-in drivers are described in Appendix E.

The BIF `open_port/2` is documented in the `m:erlang` manual page in ERTS.

For linked-in drivers, the programmer needs to read the `m:erl_ddll` manual page in Kernel.

*Examples:* Port example in [Ports](c_port.md).

## C and Java Libraries

### Erl_Interface

The program at the other side of a port is often a C program. To help the C programmer, the Erl_Interface library has been developed

The Erlang external term format is a representation of an Erlang term as a sequence of bytes, that is, a binary. Conversion between the two representations is done using the following BIFs:

```text
Binary = term_to_binary(Term)
Term = binary_to_term(Binary)
```

A port can be set to use binaries instead of lists of bytes. It is then not necessary to invent any encoding/decoding scheme. Erl_Interface functions are used for unpacking the binary and convert it into a struct similar to an Erlang term. Such a struct can be manipulated in different ways, be converted to the Erlang external format, and sent to Erlang.

*When to use:* In C code, in conjunction with Erlang binaries.

*Where to read more:* See the Erlang Interface User's Guide, Command Reference, and Library Reference. In Erlang/OTP R5B, and earlier versions, the information is part of the Kernel application.

*Examples:* Erl_Interface example in [Erl_Interface](erl_interface.md).

[](){: id=cnode }
### C Nodes

A C program that uses the Erl_Interface functions for setting up a connection to, and communicating with, a distributed Erlang node is called a *C node*, or a *hidden node*. The main advantage with a C node is that the communication from the Erlang programmer's perspective is extremely easy, as the C program behaves as a distributed Erlang node.

*When to use:* C nodes can typically be used on device processors (as opposed to control processors) where C is a better choice than Erlang due to memory limitations or application characteristics, or both.

*Where to read more:* See the `ei_connect` part of the [Erl_Interface](erl_interface.md) documentation. The programmer also needs to be familiar with TCP/IP sockets, see Sockets in [Standard Protocols](overview.md#sockets) and Distributed Erlang in [Built-In Mechanisms](overview.md#dist).

*Example:* C node example in [C Nodes](cnode.md).

### Jinterface

In Erlang/OTP R6B, a library similar to Erl_Interface for Java was added called *jinterface*. It provides a tool for Java programs to communicate with Erlang nodes.

## Standard Protocols

Sometimes communication between an Erlang program and another program using a standard protocol is desirable. Erlang/OTP currently supports TCP/IP and UDP *sockets*: as follows:

* SNMP
* HTTP
* IIOP (CORBA)

Using one of the latter three requires good knowledge about the protocol and is not covered by this tutorial. See the SNMP, Inets, and Orber applications, respectively.

[](){: id=sockets }
### Sockets

Simply put, connection-oriented socket communication (TCP/IP) consists of an initiator socket ("server") started at a certain host with a certain port number. A connector socket ("client"), which is aware of the initiator host name and port number, can connect to it and data can be sent between them.

Connection-less socket communication (UDP) consists of an initiator socket at a certain host with a certain port number and a connector socket sending data to it.

For a detailed description of the socket concept, refer to a suitable book about network programming. A suggestion is *UNIX Network Programming, Volume 1: Networking APIs - Sockets and XTI* by W. Richard Stevens, ISBN: 013490012X.

In Erlang/OTP, access to TCP/IP and UDP sockets is provided by the modules `gen_tcp` and `gen_udp` in Kernel. Both are easy to use and do not require detailed knowledge about the socket concept.

*When to use:* For programs running on the same or on another machine than the Erlang program.

*Where to read more:* See the `m:gen_tcp` and the `m:gen_udp` manual pages in Kernel.

## IC and CORBA

IC (Erlang IDL Compiler) is an interface generator that, given an IDL interface specification, automatically generates stub code in Erlang, C, or Java. See the IC User's Guide and IC Reference Manual.

For details, see the [corba repository](https://github.com/erlang/corba).

## Old Applications

Two old applications are of interest regarding interoperability. Both have been replaced by IC and are mentioned here for reference only:

* IG - Removed from Erlang/OTP R6B.

  IG (Interface Generator) automatically generated code for port or socket communication between an Erlang program and a C program, given a C header file with certain keywords.
* Jive - Removed from Erlang/OTP R7B.

  Jive provided a simple interface between an Erlang program and a Java program.
