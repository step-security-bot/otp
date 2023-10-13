# Transport mechanisms

## Callback interface

The callback interface of the transport module contains several functions. Some of which are mandatory while others are only optional:

* `send_message` \- Send a message. *Mandatory*
* `block` \- Block the transport. *Optional*

  This function is usefull for flow control.
* `unblock` \- Unblock the transport. *Optional*

For more detail, see the `m:megaco_transport` behaviour definition.

## Examples

The Megaco/H.248 application contains implementations for the two protocols specified by the Megaco/H.248 standard; UDP, see `m:megaco_udp`, and TCP/TPKT, see `m:megaco_tcp`.
