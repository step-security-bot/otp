# Socket Usage

## Introduction

The socket interface (module) is basically a "thin" layer on top of the OS socket interface. It is assumed that, unless you have special needs, gen_\[tcp|udp|sctp] should be sufficient (when they become available).

Note that just because we have a documented and described option, it does *not* mean that the OS supports it. So its recommended that the user reads the platform specific documentation for the option used.

### Asynchronous calls

Some functions allow for an *asynchronous* call ([`accept/2`](`m:socket#accept-nowait`), [`connect/3`](`m:socket#connect-nowait`), [`recv/3,4`](`m:socket#recv-nowait`), [`recvfrom/3,4`](`m:socket#recvfrom-nowait`), [`recvmsg/2,3,5`](`m:socket#recvmsg-nowait`), [`send/3,4`](`m:socket#send-nowait`), [`sendmsg/3,4`](`m:socket#sendmsg-nowait`) and [`sendto/4,5`](`m:socket#sendto-nowait`)). This is achieved by setting the `Timeout` argument to `nowait`. For instance, if calling the [`recv/3`](`m:socket#recv-nowait`) function with Timeout set to `nowait` (i.e. `recv(Sock, 0, nowait)`) when there is actually nothing to read, it will return with:

* __On Unix__ - `{select, `[`SelectInfo`](`t:socket:select_info/0`)`}`

  `SelectInfo` contains the [`SelectHandle`](`t:socket:select_handle/0`).

* __On Windows__ - `{completion, `[`CompletionInfo`](`t:socket:completion_info/0`)`}`

  `CompletionInfo` contains the [`CompletionHandle`](`t:socket:completion_handle/0`).

When data eventually arrives a 'select' or 'completion' message will be sent to the caller:

* __On Unix__ - `{'$socket', socket(), select, SelectHandle}`

  The caller can then make another call to the recv function and now expect data.

  Note that all other users are *locked out* until the 'current user' has called the function (recv in this case). So either immediately call the function or [`cancel`](`socket:cancel/2`).

* __On Windows__ - `{'$socket', socket(), completion, {CompletionHandle, CompletionStatus}}`

  The `CompletionStatus` contains the result of the operation (read).

The user must also be prepared to receive an abort message:

* ____ - `{'$socket', socket(), abort, Info}`

If the operation is aborted for whatever reason (e.g. if the socket is closed "by someone else"). The `Info` part contains the abort reason (in this case that the socket has been closed `Info = {SelectHandle, closed}`).

The general form of the 'socket' message is:

* ____ - `{'$socket', Sock :: socket(), Tag :: atom(), Info :: term()}`

Where the format of `Info` is a function of `Tag`:

| *Tag* | *Info value type* |
|-------|-------------------|
| select | select_handle() |
| completion | \{completion_handle(), CompletionStatus\} |
| abort | \{select_handle(), Reason :: term()\} |


*Table: socket message info value type*

The `select_handle()` is the same as was returned in the [`SelectInfo`](`t:socket:select_info/0`).

The `completion_handle()` is the same as was returned in the [`CompletionInfo`](`t:socket:completion_info/0`).

## Socket Registry

The *socket registry* is how we keep track of sockets. There are two functions that can be used for interaction: `socket:number_of/0` and `socket:which_sockets/1`.

In systems which create and delete *many* sockets dynamically, it (the socket registry) could become a bottleneck. For such systems, there are a couple of ways to control the use of the socket registry.

Firstly, its possible to effect the global default value when building OTP from source with the two configure options:

```text
--enable-esock-socket-registry (default) | --disable-esock-socket-registry
```

Second, its possible to effect the global default value by setting the environment variable `ESOCK_USE_SOCKET_REGISTRY` (boolean) before starting the erlang.

Third, its possible to alter the global default value in runtime by calling the function [`use_registry/1`](`socket:use_registry/1`).

And finally, its possible to override the global default when creating a socket (with [`open/2`](`socket:open/2`) and [`open/4`](`socket:open/4`)) by providing the attribute `use_registry` (boolean) in the their `Opts` argument (which effects *that* specific socket).

[](){: id=socket_options }
## Socket Options

Options for level `otp`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| assoc_id | integer() | no | yes | type = seqpacket, protocol = sctp, is an association |
| debug | boolean() | yes | yes | none |
| iow | boolean() | yes | yes | none |
| controlling_process | pid() | yes | yes | none |
| rcvbuf | default \| pos_integer() \| \{pos_integer(), pos_ineteger()\} | yes | yes | The tuple format is *not* allowed on Windows. 'default' only valid for set. The tuple form is only valid for type 'stream' and protocol 'tcp'. |
| rcvctrlbuf | default \| pos_integer() | yes | yes | default only valid for set |
| sndctrlbuf | default \| pos_integer() | yes | yes | default only valid for set |
| fd | integer() | no | yes | none |
| use_registry | boolean() | no | yes | the value is set when the socket is created, by a call to [`open/2`](`socket:open/2`) or [`open/4`](`socket:open/4`). |


*Table: option levels*

Options for level `socket`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| acceptconn | boolean() | no | yes | none |
| bindtodevice | string() | yes | yes | Before Linux 3.8, this socket option could be set, but not get. Only works for some socket types (e.g. `inet`). If empty value is set, the binding is removed. |
| broadcast | boolean() | yes | yes | type = dgram |
| bsp_state | map() | no | yes | Windows only |
| debug | integer() | yes | yes | may require admin capability |
| domain | domain() | no | yes | *Not* on FreeBSD (for instance) |
| dontroute | boolean() | yes | yes | none |
| exclusiveaddruse | boolean() | yes | yes | Windows only |
| keepalive | boolean() | yes | yes | none |
| linger | abort \| linger() | yes | yes | none |
| maxdg | integer() | no | yes | Windows only |
| max_msg_size | integer() | no | yes | Windows only |
| oobinline | boolean() | yes | yes | none |
| peek_off | integer() | yes | yes | domain = local (unix). Currently disabled due to a possible infinite loop when calling recv(\[peek]) the second time. |
| priority | integer() | yes | yes | none |
| protocol | protocol() | no | yes | *Not* on (some) Darwin (for instance) |
| rcvbuf | non_neg_integer() | yes | yes | none |
| rcvlowat | non_neg_integer() | yes | yes | none |
| rcvtimeo | timeval() | yes | yes | This option is not normally supported (see why below). OTP has to be explicitly built with the `--enable-esock-rcvsndtime` configure option for this to be available. Since our implementation is *nonblocking*, its unknown if and how this option works, or even if it may cause malfunctions. Therefore, we do not recommend setting this option. Instead, use the `Timeout` argument to, for instance, the [`recv/3`](`socket:recv/3`) function. |
| reuseaddr | boolean() | yes | yes | none |
| reuseport | boolean() | yes | yes | domain = inet \| inet6 |
| sndbuf | non_neg_integer() | yes | yes | none |
| sndlowat | non_neg_integer() | yes | yes | not changeable on Linux |
| sndtimeo | timeval() | yes | yes | This option is not normally supported (see why below). OTP has to be explicitly built with the `--enable-esock-rcvsndtime` configure option for this to be available. Since our implementation is *nonblocking*, its unknown if and how this option works, or even if it may cause malfunctions. Therefore, we do not recommend setting this option. Instead, use the `Timeout` argument to, for instance, the [`send/3`](`socket:send/3`) function. |
| timestamp | boolean() | yes | yes | none |
| type | type() | no | yes | none |


*Table: socket options*

Options for level `ip`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| add_membership | ip_mreq() | yes | no | none |
| add_source_membership | ip_mreq_source() | yes | no | none |
| block_source | ip_mreq_source() | yes | no | none |
| drop_membership | ip_mreq() | yes | no | none |
| drop_source_membership | ip_mreq_source() | yes | no | none |
| freebind | boolean() | yes | yes | none |
| hdrincl | boolean() | yes | yes | type = raw |
| minttl | integer() | yes | yes | type = raw |
| msfilter | null \| ip_msfilter() | yes | no | none |
| mtu | integer() | no | yes | type = raw |
| mtu_discover | ip_pmtudisc() | yes | yes | none |
| multicast_all | boolean() | yes | yes | none |
| multicast_if | any \| ip4_address() | yes | yes | none |
| multicast_loop | boolean() | yes | yes | none |
| multicast_ttl | uint8() | yes | yes | none |
| nodefrag | boolean() | yes | yes | type = raw |
| pktinfo | boolean() | yes | yes | type = dgram |
| recvdstaddr | boolean() | yes | yes | type = dgram |
| recverr | boolean() | yes | yes | none |
| recvif | boolean() | yes | yes | type = dgram \| raw |
| recvopts | boolean() | yes | yes | type =/= stream |
| recvorigdstaddr | boolean() | yes | yes | none |
| recvttl | boolean() | yes | yes | type =/= stream |
| retopts | boolean() | yes | yes | type =/= stream |
| router_alert | integer() | yes | yes | type = raw |
| sendsrcaddr | boolean() | yes | yes | none |
| tos | ip_tos() | yes | yes | some high-priority levels may require superuser capability |
| transparent | boolean() | yes | yes | requires admin capability |
| ttl | integer() | yes | yes | none |
| unblock_source | ip_mreq_source() | yes | no | none |


*Table: ip options*

Options for level `ipv6`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| addrform | inet | yes | no | allowed only for IPv6 sockets that are connected and bound to a v4-mapped-on-v6 address |
| add_membership | ipv6_mreq() | yes | no | none |
| authhdr | boolean() | yes | yes | type = dgram \| raw, obsolete? |
| drop_membership | ipv6_mreq() | yes | no | none |
| dstopts | boolean() | yes | yes | type = dgram \| raw, requires superuser privileges to update |
| flowinfo | boolean() | yes | yes | type = dgram \| raw, requires superuser privileges to update |
| hoplimit | boolean() | yes | yes | type = dgram \| raw. On some platforms (e.g. FreeBSD) is used to set in order to get `hoplimit` as a control message heeader. On others (e.g. Linux), `recvhoplimit` is set in order to get `hoplimit`. |
| hopopts | boolean() | yes | yes | type = dgram \| raw, requires superuser privileges to update |
| mtu | boolean() | yes | yes | Get: Only after the socket has been connected |
| mtu_discover | ipv6_pmtudisc() | yes | yes | none |
| multicast_hops | default \| uint8() | yes | yes | none |
| multicast_if | integer() | yes | yes | type = dgram \| raw |
| multicast_loop | boolean() | yes | yes | none |
| recverr | boolean() | yes | yes | none |
| recvhoplimit | boolean() | yes | yes | type = dgram \| raw. On some platforms (e.g. Linux), `recvhoplimit` is set in order to get `hoplimit` |
| recvpktinfo \| pktinfo | boolean() | yes | yes | type = dgram \| raw. On some platforms (e.g. FreeBSD) is used to set in order to get `hoplimit` as a control message heeader. On others (e.g. Linux), `recvhoplimit` is set in order to get `hoplimit`. |
| recvtclass | boolean() | yes | yes | type = dgram \| raw. On some platforms is used to set (=true) in order to get the `tclass` control message heeader. On others, `tclass` is set in order to get `tclass` control message heeader. |
| router_alert | integer() | yes | yes | type = raw |
| rthdr | boolean() | yes | yes | type = dgram \| raw, requires superuser privileges to update |
| tclass | integer() | yes | yes | Set the traffic class associated with outgoing packets. RFC3542. |
| unicast_hops | default \| uint8() | yes | yes | none |
| v6only | boolean() | yes | no | none |


*Table: ipv6 options*

Options for level `tcp`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| congestion | string() | yes | yes | none |
| cork | boolean() | yes | yes | 'nopush' one some platforms (FreeBSD) |
| keepcnt | integer() | yes | yes | On Windows (at least), it is illegal to set to a value greater than 255. |
| keepidle | integer() | yes | yes | none |
| keepintvl | integer() | yes | yes | none |
| maxseg | integer() | yes | yes | Set not allowed on all platforms. |
| nodelay | boolean() | yes | yes | none |
| nopush | boolean() | yes | yes | 'cork' on some platforms (Linux). On Darwin this has a different meaning than on, for instance, FreeBSD. |


*Table: tcp options*

Options for level `udp`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| cork | boolean() | yes | yes | none |


*Table: udp options*

Options for level `sctp`:

| *Option Name* | *Value Type* | *Set* | *Get* | *Other Requirements and comments* |
|---------------|--------------|-------|-------|-----------------------------------|
| associnfo | sctp_assocparams() | yes | yes | none |
| autoclose | non_neg_integer() | yes | yes | none |
| disable_fragments | boolean() | yes | yes | none |
| events | sctp_event_subscribe() | yes | no | none |
| initmsg | sctp_initmsg() | yes | yes | none |
| maxseg | non_neg_integer() | yes | yes | none |
| nodelay | boolean() | yes | yes | none |
| rtoinfo | sctp_rtoinfo() | yes | yes | none |


*Table: sctp options*
