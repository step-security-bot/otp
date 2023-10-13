# Communication in Erlang

Communication in Erlang is conceptually performed using asynchronous signaling. All different executing entities, such as processes and ports, communicate through asynchronous signals. The most commonly used signal is a message. Other common signals are exit, link, unlink, monitor, and demonitor signals.

## Passing of Signals

This information has been moved to the [*Signals* section of the *Processes* chapter in the *Erlang Reference Manual*](`p:system:ref_man_processes.md#signal-delivery`).

## Synchronous Communication

This information has been moved to the [*Signals* section of the *Processes* chapter in the *Erlang Reference Manual*](`p:system:ref_man_processes.md#sync-comm`).

## Implementation

The implementation of different asynchronous signals in the virtual machine can vary over time, but the behavior always respects this concept of asynchronous signals being passed between entities as described above.

By inspecting the implementation, you might notice that some specific signal gives a stricter guarantee than described above. It is of vital importance that such knowledge about the implementation is *not* used by Erlang code, as the implementation can change at any time without prior notice.

Examples of major implementation changes:

* As from ERTS 5.5.2 exit signals to processes are truly asynchronously delivered.
* As from ERTS 5.10 all signals from processes to ports are truly asynchronously delivered.
