# erl_driver

API functions for an Erlang driver.

## Description

An Erlang driver is a library containing a set of native driver callback functions that the Erlang Virtual Machine calls when certain events occur. There can be multiple instances of a driver, each instance is associated with an Erlang port.

[](){: id=WARNING }
> #### Warning {: class=warning }
> *Use this functionality with extreme care.*
>
> A driver callback is executed as a direct extension of the native code of the VM. Execution is not made in a safe environment. The VM *cannot* provide the same services as provided when executing Erlang code, such as pre-emptive scheduling or memory protection. If the driver callback function does not behave well, the whole VM will misbehave.
>
> * A driver callback that crash will crash the whole VM.
> * An erroneously implemented driver callback can cause a VM internal state inconsistency, which can cause a crash of the VM, or miscellaneous misbehaviors of the VM at any point after the call to the driver callback.
> * A driver callback doing [lengthy work](erl_driver.md#lengthy_work) before returning degrades responsiveness of the VM and can cause miscellaneous strange behaviors. Such strange behaviors include, but are not limited to, extreme memory usage and bad load balancing between schedulers. Strange behaviors that can occur because of lengthy work can also vary between Erlang/OTP releases.

As from ERTS 5.5.3 the driver interface has been extended (see [`extended marker`](driver_entry.md#extended_marker)). The extended interface introduces [version management](erl_driver.md#version_management), the possibility to pass capability flags (see [`driver_flags`](driver_entry.md#driver_flags)) to the runtime system at driver initialization, and some new driver API functions.

> #### Note {: class=info }
> As from ERTS 5.9 old drivers must be recompiled and use the extended interface. They must also be adjusted to the [64-bit capable driver interface](erl_driver.md#rewrites_for_64_bits).

The driver calls back to the emulator, using the API functions declared in `erl_driver.h`. They are used for outputting data from the driver, using timers, and so on.

Each driver instance is associated with a port. Every port has a port owner process. Communication with the port is normally done through the port owner process. Most of the functions take the `port` handle as an argument. This identifies the driver instance. Notice that this port handle must be stored by the driver, it is not given when the driver is called from the emulator (see [`driver_entry`](driver_entry.md#emulator)).

Some of the functions take a parameter of type `ErlDrvBinary`, a driver binary. It is to be both allocated and freed by the caller. Using a binary directly avoids one extra copying of data.

Many of the output functions have a "header buffer", with `hbuf` and `hlen` parameters. This buffer is sent as a list before the binary (or list, depending on port mode) that is sent. This is convenient when matching on messages received from the port. (Although in the latest Erlang versions there is the binary syntax, which enables you to match on the beginning of a binary.)

[](){: id=smp_support }
Drivers are locked either on driver level or port level (driver instance level). By default driver level locking will be used, that is, only one emulator thread will execute code in the driver at a time. If port level locking is used, multiple emulator threads can execute code in the driver at the same time. Only one thread at a time will call driver callbacks corresponding to the same port, though. To enable port level locking, set the `ERL_DRV_FLAG_USE_PORT_LOCKING` [driver flag](driver_entry.md#driver_flags) in the [`driver_entry`](driver_entry.md) used by the driver. When port level locking is used, the driver writer is responsible for synchronizing all accesses to data shared by the ports (driver instances).

Most drivers written before the runtime system with SMP support existed can run in the runtime system with SMP support, without being rewritten, if driver level locking is used.

> #### Note {: class=info }
> It is assumed that drivers do not access other drivers. If drivers access each other, they must provide their own mechanism for thread-safe synchronization. Such "inter-driver communication" is strongly discouraged.

> #### Note {: class=info }
> Regardless of locking scheme used, calls to driver callbacks can be made from different threads.

Most functions in this API are *not* thread-safe, that is, they *cannot* be called from arbitrary threads. Functions that are not documented as thread-safe can only be called from driver callbacks or function calls descending from a driver callback call. Notice that driver callbacks can be called from different threads. This, however, is not a problem for any function in this API, as the emulator has control over these threads.

> #### Warning {: class=warning }
> Functions not explicitly documented as thread-safe are *not* thread safe.
>
> A function not explicitly documented as thread-safe can, at some point in time, have a thread-safe implementation in the runtime system. Such an implementation can however change to a thread *unsafe* implementation at any time *without any notice*.
>
> *Only use functions explicitly documented as thread-safe from arbitrary threads.*

[](){: id=lengthy_work }
As mentioned in the [warning](erl_driver.md#warning) text at the beginning of this section, it is of vital importance that a driver callback returns relatively fast. It is difficult to give an exact maximum amount of time that a driver callback is allowed to work, but usually a well-behaving driver callback is to return within 1 millisecond. This can be achieved using different approaches. If you have full control over the code to execute in the driver callback, the best approach is to divide the work into multiple chunks of work, and trigger multiple calls to the [time-out callback](driver_entry.md#timeout) using zero time-outs. Function [`erl_drv_consume_timeslice`](erl_driver.md#erl_drv_consume_timeslice) can be useful to determine when to trigger such time-out callback calls. However, sometimes it cannot be implemented this way, for example when calling third-party libraries. In this case, you typically want to dispatch the work to another thread. Information about thread primitives is provided below.

## Functionality

All functions that a driver needs to do with Erlang are performed through driver API functions. Functions exist for the following functionality:

* __Timer functions__ - Control the timer that a driver can use. The timer has the emulator call the [`timeout`](driver_entry.md#timeout) entry function after a specified time. Only one timer is available for each driver instance.

* __Queue handling__ - Every driver instance has an associated queue. This queue is a `SysIOVec`, which works as a buffer. It is mostly used for the driver to buffer data that is to be written to a device, it is a byte stream. If the port owner process closes the driver, and the queue is not empty, the driver is not closed. This enables the driver to flush its buffers before closing.

  The queue can be manipulated from any threads if a port data lock is used. For more information, see [`ErlDrvPDL`](erl_driver.md#erldrvpdl).

* __Output functions__ - With these functions, the driver sends data back to the emulator. The data is received as messages by the port owner process, see `erlang:open_port/2`. The vector function and the function taking a driver binary are faster, as they avoid copying the data buffer. There is also a fast way of sending terms from the driver, without going through the binary term format.

* __Failure__ - The driver can exit and signal errors up to Erlang. This is only for severe errors, when the driver cannot possibly keep open.

* __Asynchronous calls__ - Erlang/OTP R7B and later versions have provision for asynchronous function calls, using a thread pool provided by Erlang. There is also a select call, which can be used for asynchronous drivers.

* __[](){: id=multi_threading }
  Multi-threading__  
  A POSIX thread like API for multi-threading is provided. The Erlang driver thread API only provides a subset of the functionality provided by the POSIX thread API. The subset provided is more or less the basic functionality needed for multi-threaded programming:

  * [Threads](erl_driver.md#erldrvtid)
  * [Mutexes](erl_driver.md#erldrvmutex)
  * [Condition variables](erl_driver.md#erldrvcond)
  * [Read/write locks](erl_driver.md#erldrvrwlock)
  * [Thread-specific data](erl_driver.md#erldrvtsdkey)

  The Erlang driver thread API can be used in conjunction with the POSIX thread API on UN-ices and with the Windows native thread API on Windows. The Erlang driver thread API has the advantage of being portable, but there can exist situations where you want to use functionality from the POSIX thread API or the Windows native thread API.

  The Erlang driver thread API only returns error codes when it is reasonable to recover from an error condition. If it is not reasonable to recover from an error condition, the whole runtime system is terminated. For example, if a create mutex operation fails, an error code is returned, but if a lock operation on a mutex fails, the whole runtime system is terminated.

  Notice that there is no "condition variable wait with time-out" in the Erlang driver thread API. This because of issues with `pthread_cond_timedwait`. When the system clock suddenly is changed, it is not always guaranteed that you will wake up from the call as expected. An Erlang runtime system must be able to cope with sudden changes of the system clock. Therefore, we have omitted it from the Erlang driver thread API. In the Erlang driver case, time-outs can and are to be handled with the timer functionality of the Erlang driver API.

  Notice that many functions in the Erlang driver API are *not* thread-safe. If a function is not documented as thread-safe, it is *not* thread-safe.

  > #### Note {: class=info }
  > When executing in an emulator thread, it is *very important* that you unlock *all* locks you have locked before letting the thread out of your control; otherwise you are *very likely* to deadlock the whole emulator.
  >
  > If you need to use thread-specific data in an emulator thread, only have the thread-specific data set while the thread is under your control, and clear the thread-specific data before you let the thread out of your control.

  In the future, debug functionality will probably be integrated with the Erlang driver thread API. All functions that create entities take a `name` argument. Currently the `name` argument is unused, but it will be used when the debug functionality is implemented. If you name all entities created well, the debug functionality will be able to give you better error reports.

* __Adding/removing drivers__ - A driver can add and later remove drivers.

* __Monitoring processes__ - A driver can monitor a process that does not own a port.

* __[](){: id=version_management }
  Version management__  
  Version management is enabled for drivers that have set the [`extended_marker`](driver_entry.md#extended_marker) field of their [`driver_entry`](driver_entry.md) to `ERL_DRV_EXTENDED_MARKER`. `erl_driver.h` defines:

  * `ERL_DRV_EXTENDED_MARKER`
  * `ERL_DRV_EXTENDED_MAJOR_VERSION`, which is incremented when driver incompatible changes are made to the Erlang runtime system. Normally it suffices to recompile drivers when `ERL_DRV_EXTENDED_MAJOR_VERSION` has changed, but it can, under rare circumstances, mean that drivers must be slightly modified. If so, this will of course be documented.
  * `ERL_DRV_EXTENDED_MINOR_VERSION`, which is incremented when new features are added. The runtime system uses the minor version of the driver to determine what features to use.

  The runtime system normally refuses to load a driver if the major versions differ, or if the major versions are equal and the minor version used by the driver is greater than the one used by the runtime system. Old drivers with lower major versions are however allowed after a bump of the major version during a transition period of two major releases. Such old drivers can, however, fail if deprecated features are used.

  The emulator refuses to load a driver that does not use the extended driver interface, to allow for 64-bit capable drivers, as incompatible type changes for the callbacks [`output`](driver_entry.md#output), [`control`](driver_entry.md#control), and [`call`](driver_entry.md#call) were introduced in Erlang/OTP R15B. A driver written with the old types would compile with warnings and when called return garbage sizes to the emulator, causing it to read random memory and create huge incorrect result blobs.

  Therefore it is not enough to only recompile drivers written with version management for pre R15B types; the types must be changed in the driver suggesting other rewrites, especially regarding size variables. *Investigate all warnings when recompiling.*

  Also, the API driver functions `driver_output*` and `driver_vec_to_buf`, `driver_alloc/realloc*`, and the `driver_*` queue functions were changed to have larger length arguments and return values. This is a lesser problem, as code that passes smaller types gets them auto-converted in the calls, and as long as the driver does not handle sizes that overflow an `int`, all will work as before.

* __[](){: id=time_measurement }
  Time measurement__  
  Support for time measurement in drivers:

  * [`ErlDrvTime`](erl_driver.md#erldrvtime)
  * [`ErlDrvTimeUnit`](erl_driver.md#erldrvtimeunit)
  * [`erl_drv_monotonic_time`](erl_driver.md#erl_drv_monotonic_time)
  * [`erl_drv_time_offset`](erl_driver.md#erl_drv_time_offset)
  * [`erl_drv_convert_time_unit`](erl_driver.md#erl_drv_convert_time_unit)

[](){: id=rewrites_for_64_bits }
## Rewrites for 64-Bit Driver Interface

ERTS 5.9 introduced two new integer types, [`ErlDrvSizeT`](erl_driver.md#erldrvsizet) and [`ErlDrvSSizeT`](erl_driver.md#erldrvssizet), which can hold 64-bit sizes if necessary.

To not update a driver and only recompile, it probably works when building for a 32-bit machine creating a false sense of security. Hopefully that will generate many important warnings. But when recompiling the same driver later on for a 64-bit machine, there *will* be warnings and almost certainly crashes. So it is a *bad* idea to postpone updating the driver and not fixing the warnings.

When recompiling with `gcc`, use flag `-Wstrict-prototypes` to get better warnings. Try to find a similar flag if you use another compiler.

The following is a checklist for rewriting a pre ERTS 5.9 driver, most important first:

* __Return types for driver callbacks__ - Rewrite driver callback [`control`](driver_entry.md#control) to use return type `ErlDrvSSizeT` instead of `int`.

  Rewrite driver callback [`call`](driver_entry.md#call) to use return type `ErlDrvSSizeT` instead of `int`.

  > #### Note {: class=info }
  > These changes are essential not to crash the emulator or worse cause malfunction. Without them a driver can return garbage in the high 32 bits to the emulator, causing it to build a huge result from random bytes, either crashing on memory allocation or succeeding with a random result from the driver call.

* __Arguments to driver callbacks__ - Driver callback [`output`](driver_entry.md#output) now gets `ErlDrvSizeT` as 3rd argument instead of previously `int`.

  Driver callback [`control`](driver_entry.md#control) now gets `ErlDrvSizeT` as 4th and 6th arguments instead of previously `int`.

  Driver callback [`call`](driver_entry.md#call) now gets `ErlDrvSizeT` as 4th and 6th arguments instead of previously `int`.

  Sane compiler's calling conventions probably make these changes necessary only for a driver to handle data chunks that require 64-bit size fields (mostly larger than 2 GB, as that is what an `int` of 32 bits can hold). But it is possible to think of non-sane calling conventions that would make the driver callbacks mix up the arguments causing malfunction.

  > #### Note {: class=info }
  > The argument type change is from signed to unsigned. This can cause problems for, for example, loop termination conditions or error conditions if you only change the types all over the place.

* __Larger `size` field in `ErlIOVec`__ - The `size` field in [`ErlIOVec`](erl_driver.md#erliovec) has been changed to `ErlDrvSizeT` from `int`. Check all code that use that field.

  Automatic type-casting probably makes these changes necessary only for a driver that encounters sizes > 32 bits.

  > #### Note {: class=info }
  > The `size` field changed from signed to unsigned. This can cause problems for, for example, loop termination conditions or error conditions if you only change the types all over the place.

* __Arguments and return values in the driver API__ - Many driver API functions have changed argument type and/or return value to `ErlDrvSizeT` from mostly `int`. Automatic type-casting probably makes these changes necessary only for a driver that encounters sizes > 32 bits.

  * __[`driver_output`](erl_driver.md#driver_output)__ - 3rd argument

  * __[`driver_output2`](erl_driver.md#driver_output2)__ - 3rd and 5th arguments

  * __[`driver_output_binary`](erl_driver.md#driver_output_binary)__ - 3rd, 5th, and 6th arguments

  * __[`driver_outputv`](erl_driver.md#driver_outputv)__ - 3rd and 5th arguments

  * __[`driver_vec_to_buf`](erl_driver.md#driver_vec_to_buf)__ - 3rd argument and return value

  * __[`driver_alloc`](erl_driver.md#driver_alloc)__ - 1st argument

  * __[`driver_realloc`](erl_driver.md#driver_realloc)__ - 2nd argument

  * __[`driver_alloc_binary`](erl_driver.md#driver_alloc_binary)__ - 1st argument

  * __[`driver_realloc_binary`](erl_driver.md#driver_realloc_binary)__ - 2nd argument

  * __[`driver_enq`](erl_driver.md#driver_enq)__ - 3rd argument

  * __[`driver_pushq`](erl_driver.md#driver_pushq)__ - 3rd argument

  * __[`driver_deq`](erl_driver.md#driver_deq)__ - 2nd argument and return value

  * __[`driver_sizeq`](erl_driver.md#driver_sizeq)__ - Return value

  * __[`driver_enq_bin`](erl_driver.md#driver_enq_bin)__ - 3rd and 4th arguments

  * __[`driver_pushq_bin`](erl_driver.md#driver_pushq_bin)__ - 3rd and 4th arguments

  * __[`driver_enqv`](erl_driver.md#driver_enqv)__ - 3rd argument

  * __[`driver_pushqv`](erl_driver.md#driver_pushqv)__ - 3rd argument

  * __[`driver_peekqv`](erl_driver.md#driver_peekqv)__ - Return value

  > #### Note {: class=info }
  > This is a change from signed to unsigned. This can cause problems for, for example, loop termination conditions and error conditions if you only change the types all over the place.

## Data Types

* __`ErlDrvSizeT`{: id=ErlDrvSizeT }__ - An unsigned integer type to be used as `size_t`.

* __`ErlDrvSSizeT`{: id=ErlDrvSSizeT }__ - A signed integer type, the size of `ErlDrvSizeT`.

* __`ErlDrvSysInfo`{: id=ErlDrvSysInfo }__  
  ```text
  typedef struct ErlDrvSysInfo {
     int driver_major_version;
     int driver_minor_version;
     char *erts_version;
     char *otp_release;
     int thread_support;
     int smp_support;
     int async_threads;
     int scheduler_threads;
     int nif_major_version;
     int nif_minor_version;
     int dirty_scheduler_support;
  } ErlDrvSysInfo;
  ```

  The `ErlDrvSysInfo` structure is used for storage of information about the Erlang runtime system. [`driver_system_info`](erl_driver.md#driver_system_info) writes the system information when passed a reference to a `ErlDrvSysInfo` structure. The fields in the structure are as follows:

  * __`driver_major_version`__ - The value of [`ERL_DRV_EXTENDED_MAJOR_VERSION`](erl_driver.md#version_management) when the runtime system was compiled. This value is the same as the value of [`ERL_DRV_EXTENDED_MAJOR_VERSION`](erl_driver.md#version_management) used when compiling the driver; otherwise the runtime system would have refused to load the driver.

  * __`driver_minor_version`__ - The value of [`ERL_DRV_EXTENDED_MINOR_VERSION`](erl_driver.md#version_management) when the runtime system was compiled. This value can differ from the value of [`ERL_DRV_EXTENDED_MINOR_VERSION`](erl_driver.md#version_management) used when compiling the driver.

  * __`erts_version`__ - A string containing the version number of the runtime system (the same as returned by [`erlang:system_info(version)`](`m:erlang#system_info_version`)).

  * __`otp_release`__ - A string containing the OTP release number (the same as returned by [`erlang:system_info(otp_release)`](`m:erlang#system_info_otp_release`)).

  * __`thread_support`__ - A value `!= 0` if the runtime system has thread support; otherwise `0`.

  * __`smp_support`__ - A value `!= 0` if the runtime system has SMP support; otherwise `0`.

  * __`async_threads`__ - The number of async threads in the async thread pool used by [`driver_async`](erl_driver.md#driver_async) (the same as returned by [`erlang:system_info(thread_pool_size)`](`m:erlang#system_info_thread_pool_size`)).

  * __`scheduler_threads`__ - The number of scheduler threads used by the runtime system (the same as returned by [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`)).

  * __`nif_major_version`__ - The value of `ERL_NIF_MAJOR_VERSION` when the runtime system was compiled.

  * __`nif_minor_version`__ - The value of `ERL_NIF_MINOR_VERSION` when the runtime system was compiled.

  * __`dirty_scheduler_support`__ - A value `!= 0` if the runtime system has support for dirty scheduler threads; otherwise `0`.

  

* __`ErlDrvBinary`{: id=ErlDrvBinary }__  
  ```text
  typedef struct ErlDrvBinary {
     ErlDrvSint orig_size;
     char orig_bytes[];
  } ErlDrvBinary;
  ```

  The `ErlDrvBinary` structure is a binary, as sent between the emulator and the driver. All binaries are reference counted; when `driver_binary_free` is called, the reference count is decremented, when it reaches zero, the binary is deallocated. `orig_size` is the binary size and `orig_bytes` is the buffer. `ErlDrvBinary` has not a fixed size, its size is `orig_size + 2 * sizeof(int)`.

  > #### Note {: class=info }
  > The `refc` field has been removed. The reference count of an `ErlDrvBinary` is now stored elsewhere. The reference count of an `ErlDrvBinary` can be accessed through [`driver_binary_get_refc`](erl_driver.md#driver_binary_get_refc), [`driver_binary_inc_refc`](erl_driver.md#driver_binary_inc_refc), and [`driver_binary_dec_refc`](erl_driver.md#driver_binary_dec_refc).

  Some driver calls, such as `driver_enq_binary`, increment the driver reference count, and others, such as `driver_deq` decrement it.

  Using a driver binary instead of a normal buffer is often faster, as the emulator needs not to copy the data, only the pointer is used.

  A driver binary allocated in the driver, with `driver_alloc_binary`, is to be freed in the driver (unless otherwise stated) with `driver_free_binary`. (Notice that this does not necessarily deallocate it, if the driver is still referred in the emulator, the ref-count will not go to zero.)

  Driver binaries are used in the `driver_output2` and `driver_outputv` calls, and in the queue. Also the driver callback [`outputv`](driver_entry.md#outputv) uses driver binaries.

  If the driver for some reason wants to keep a driver binary around, for example in a static variable, the reference count is to be incremented, and the binary can later be freed in the [`stop`](driver_entry.md#stop) callback, with `driver_free_binary`.

  Notice that as a driver binary is shared by the driver and the emulator. A binary received from the emulator or sent to the emulator must not be changed by the driver.

  Since ERTS 5.5 (Erlang/OTP R11B), `orig_bytes` is guaranteed to be properly aligned for storage of an array of doubles (usually 8-byte aligned).

* __`ErlDrvData`__ - A handle to driver-specific data, passed to the driver callbacks. It is a pointer, and is most often type cast to a specific pointer in the driver.

* __`SysIOVec`__ - A system I/O vector, as used by `writev` on Unix and `WSASend` on Win32. It is used in `ErlIOVec`.

* __`ErlIOVec`{: id=ErlIOVec }__  
  ```text
  typedef struct ErlIOVec {
    int vsize;
    ErlDrvSizeT size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
  } ErlIOVec;
  ```

  The I/O vector used by the emulator and drivers is a list of binaries, with a `SysIOVec` pointing to the buffers of the binaries. It is used in `driver_outputv` and the [`outputv`](driver_entry.md#outputv) driver callback. Also, the driver queue is an `ErlIOVec`.

* __`ErlDrvMonitor`__ - When a driver creates a monitor for a process, a `ErlDrvMonitor` is filled in. This is an opaque data type that can be assigned to, but not compared without using the supplied compare function (that is, it behaves like a struct).

  The driver writer is to provide the memory for storing the monitor when calling [`driver_monitor_process`](erl_driver.md#driver_monitor_process). The address of the data is not stored outside of the driver, so `ErlDrvMonitor` can be used as any other data, it can be copied, moved in memory, forgotten, and so on.

* __`ErlDrvNowData`{: id=ErlDrvNowData }__ - The `ErlDrvNowData` structure holds a time stamp consisting of three values measured from some arbitrary point in the past. The three structure members are:

  * __`megasecs`__ - The number of whole megaseconds elapsed since the arbitrary point in time

  * __`secs`__ - The number of whole seconds elapsed since the arbitrary point in time

  * __`microsecs`__ - The number of whole microseconds elapsed since the arbitrary point in time

  

* __`ErlDrvPDL`{: id=ErlDrvPDL }__ - If certain port-specific data must be accessed from other threads than those calling the driver callbacks, a port data lock can be used to synchronize the operations on the data. Currently, the only port-specific data that the emulator associates with the port data lock is the driver queue.

  Normally a driver instance has no port data lock. If the driver instance wants to use a port data lock, it must create the port data lock by calling [`driver_pdl_create`](erl_driver.md#driver_pdl_create).

  > #### Note {: class=info }
  > Once the port data lock has been created, every access to data associated with the port data lock must be done while the port data lock is locked. The port data lock is locked and unlocked by [`driver_pdl_lock`](erl_driver.md#driver_pdl_lock), and [`driver_pdl_unlock`](erl_driver.md#driver_pdl_unlock), respectively.

  A port data lock is reference counted, and when the reference count reaches zero, it is destroyed. The emulator at least increments the reference count once when the lock is created and decrements it once the port associated with the lock terminates. The emulator also increments the reference count when an async job is enqueued and decrements it when an async job has been invoked. Also, the driver is responsible for ensuring that the reference count does not reach zero before the last use of the lock by the driver has been made. The reference count can be read, incremented, and decremented by [`driver_pdl_get_refc`](erl_driver.md#driver_pdl_get_refc), [`driver_pdl_inc_refc`](erl_driver.md#driver_pdl_inc_refc), and [`driver_pdl_dec_refc`](erl_driver.md#driver_pdl_dec_refc), respectively.

* __`ErlDrvTid`{: id=ErlDrvTid }__ - Thread identifier.

  See also [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create), [`erl_drv_thread_exit`](erl_driver.md#erl_drv_thread_exit), [`erl_drv_thread_join`](erl_driver.md#erl_drv_thread_join), [`erl_drv_thread_self`](erl_driver.md#erl_drv_thread_self), and [`erl_drv_equal_tids`](erl_driver.md#erl_drv_equal_tids).

* __`ErlDrvThreadOpts`{: id=ErlDrvThreadOpts }__  
  ```text
  int suggested_stack_size;
  ```

  Thread options structure passed to [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create). The following field exists:

  * __`suggested_stack_size`__ - A suggestion, in kilowords, on how large a stack to use. A value < 0 means default size.

  See also [`erl_drv_thread_opts_create`](erl_driver.md#erl_drv_thread_opts_create), [`erl_drv_thread_opts_destroy`](erl_driver.md#erl_drv_thread_opts_destroy), and [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create).

* __`ErlDrvMutex`{: id=ErlDrvMutex }__ - Mutual exclusion lock. Used for synchronizing access to shared data. Only one thread at a time can lock a mutex.

  See also [`erl_drv_mutex_create`](erl_driver.md#erl_drv_mutex_create), [`erl_drv_mutex_destroy`](erl_driver.md#erl_drv_mutex_destroy), [`erl_drv_mutex_lock`](erl_driver.md#erl_drv_mutex_lock), [`erl_drv_mutex_trylock`](erl_driver.md#erl_drv_mutex_trylock), and [`erl_drv_mutex_unlock`](erl_driver.md#erl_drv_mutex_unlock).

* __`ErlDrvCond`{: id=ErlDrvCond }__ - Condition variable. Used when threads must wait for a specific condition to appear before continuing execution. Condition variables must be used with associated mutexes.

  See also [`erl_drv_cond_create`](erl_driver.md#erl_drv_cond_create), [`erl_drv_cond_destroy`](erl_driver.md#erl_drv_cond_destroy), [`erl_drv_cond_signal`](erl_driver.md#erl_drv_cond_signal), [`erl_drv_cond_broadcast`](erl_driver.md#erl_drv_cond_broadcast), and [`erl_drv_cond_wait`](erl_driver.md#erl_drv_cond_wait).

* __`ErlDrvRWLock`{: id=ErlDrvRWLock }__ - Read/write lock. Used to allow multiple threads to read shared data while only allowing one thread to write the same data. Multiple threads can read lock an rwlock at the same time, while only one thread can read/write lock an rwlock at a time.

  See also [`erl_drv_rwlock_create`](erl_driver.md#erl_drv_rwlock_create), [`erl_drv_rwlock_destroy`](erl_driver.md#erl_drv_rwlock_destroy), [`erl_drv_rwlock_rlock`](erl_driver.md#erl_drv_rwlock_rlock), [`erl_drv_rwlock_tryrlock`](erl_driver.md#erl_drv_rwlock_tryrlock), [`erl_drv_rwlock_runlock`](erl_driver.md#erl_drv_rwlock_runlock), [`erl_drv_rwlock_rwlock`](erl_driver.md#erl_drv_rwlock_rwlock), [`erl_drv_rwlock_tryrwlock`](erl_driver.md#erl_drv_rwlock_tryrwlock), and [`erl_drv_rwlock_rwunlock`](erl_driver.md#erl_drv_rwlock_rwunlock).

* __`ErlDrvTSDKey`{: id=ErlDrvTSDKey }__ - Key that thread-specific data can be associated with.

  See also [`erl_drv_tsd_key_create`](erl_driver.md#erl_drv_tsd_key_create), [`erl_drv_tsd_key_destroy`](erl_driver.md#erl_drv_tsd_key_destroy), [`erl_drv_tsd_set`](erl_driver.md#erl_drv_tsd_set), and [`erl_drv_tsd_get`](erl_driver.md#erl_drv_tsd_get).

* __`ErlDrvTime`{: id=ErlDrvTime }__ - A signed 64-bit integer type for time representation.

* __`ErlDrvTimeUnit`{: id=ErlDrvTimeUnit }__ - An enumeration of time units supported by the driver API:

  * __`ERL_DRV_SEC`__ - Seconds

  * __`ERL_DRV_MSEC`__ - Milliseconds

  * __`ERL_DRV_USEC`__ - Microseconds

  * __`ERL_DRV_NSEC`__ - Nanoseconds

  

[](){: id=add_driver_entry }
## add_driver_entry/1

```c
void add_driver_entry(ErlDrvEntry
        *de);
```

[](){: id=add_driver_entry }
Adds a driver entry to the list of drivers known by Erlang. The [`init`](driver_entry.md#init) function of parameter `de` is called.

> #### Note {: class=info }
> To use this function for adding drivers residing in dynamically loaded code is dangerous. If the driver code for the added driver resides in the same dynamically loaded module (that is, `.so` file) as a normal dynamically loaded driver (loaded with the `erl_ddll` interface), the caller is to call [`driver_lock_driver`](erl_driver.md#driver_lock_driver) before adding driver entries.
>
> *Use of this function is generally deprecated.*

[](){: id=driver_alloc }
## driver_alloc/1

```c
void * driver_alloc(ErlDrvSizeT size);
```

[](){: id=driver_alloc }
Allocates a memory block of the size specified in `size`, and returns it. This fails only on out of memory, in which case `NULL` is returned. (This is most often a wrapper for `malloc`).

Memory allocated must be explicitly freed with a corresponding call to [`driver_free`](erl_driver.md#driver_free) (unless otherwise stated).

This function is thread-safe.

[](){: id=driver_alloc_binary }
## driver_alloc_binary/1

```c
ErlDrvBinary * driver_alloc_binary(ErlDrvSizeT size);
```

[](){: id=driver_alloc_binary }
Allocates a driver binary with a memory block of at least `size` bytes, and returns a pointer to it, or `NULL` on failure (out of memory). When a driver binary has been sent to the emulator, it must not be changed. Every allocated binary is to be freed by a corresponding call to [`driver_free_binary`](erl_driver.md#driver_free_binary) (unless otherwise stated).

Notice that a driver binary has an internal reference counter. This means that calling `driver_free_binary`, it may not actually dispose of it. If it is sent to the emulator, it can be referenced there.

The driver binary has a field, `orig_bytes`, which marks the start of the data in the binary.

This function is thread-safe.

[](){: id=driver_async }
## driver_async/5

```c
long driver_async(ErlDrvPort port, unsigned
        int* key, void (*async_invoke)(void*), void* async_data, void
        (*async_free)(void*));
```

[](){: id=driver_async }
Performs an asynchronous call. The function `async_invoke` is invoked in a thread separate from the emulator thread. This enables the driver to perform time-consuming, blocking operations without blocking the emulator.

The async thread pool size can be set with command-line argument [`+A`](erl_cmd.md#async_thread_pool_size) in [`erl(1)`](erl_cmd.md). If an async thread pool is unavailable, the call is made synchronously in the thread calling `driver_async`. The current number of async threads in the async thread pool can be retrieved through [`driver_system_info`](erl_driver.md#driver_system_info).

If a thread pool is available, a thread is used. If argument `key` is `NULL`, the threads from the pool are used in a round-robin way, each call to `driver_async` uses the next thread in the pool. With argument `key` set, this behavior is changed. The two same values of `*key` always get the same thread.

To ensure that a driver instance always uses the same thread, the following call can be used:

```text
unsigned int myKey = driver_async_port_key(myPort);

r = driver_async(myPort, &myKey, myData, myFunc);
```

It is enough to initialize `myKey` once for each driver instance.

If a thread is already working, the calls are queued up and executed in order. Using the same thread for each driver instance ensures that the calls are made in sequence.

The `async_data` is the argument to the functions `async_invoke` and `async_free`. It is typically a pointer to a structure containing a pipe or event that can be used to signal that the async operation completed. The data is to be freed in `async_free`.

When the async operation is done, [`ready_async`](driver_entry.md#ready_async) driver entry function is called. If `ready_async` is `NULL` in the driver entry, the `async_free` function is called instead.

The return value is `-1` if the `driver_async` call fails.

> #### Note {: class=info }
> As from ERTS 5.5.4.3 the default stack size for threads in the async-thread pool is 16 kilowords, that is, 64 kilobyte on 32-bit architectures. This small default size has been chosen because the amount of async-threads can be quite large. The default stack size is enough for drivers delivered with Erlang/OTP, but is possibly not sufficiently large for other dynamically linked-in drivers that use the `driver_async` functionality. A suggested stack size for threads in the async-thread pool can be configured through command-line argument [`+a`](erl_cmd.md#async_thread_stack_size) in [`erl(1)`](erl_cmd.md).

[](){: id=driver_async_port_key }
## driver_async_port_key/1

```c
unsigned int driver_async_port_key(ErlDrvPort
        port);
```

[](){: id=driver_async_port_key }
Calculates a key for later use in [`driver_async`](erl_driver.md#driver_async). The keys are evenly distributed so that a fair mapping between port IDs and async thread IDs is achieved.

> #### Note {: class=info }
> Before Erlang/OTP R16, the port ID could be used as a key with proper casting, but after the rewrite of the port subsystem, this is no longer the case. With this function, you can achieve the same distribution based on port IDs as before Erlang/OTP R16.

[](){: id=driver_binary_dec_refc }
## driver_binary_dec_refc/1

```c
long driver_binary_dec_refc(ErlDrvBinary *bin);
```

[](){: id=driver_binary_dec_refc }
Decrements the reference count on `bin` and returns the reference count reached after the decrement.

This function is thread-safe.

> #### Note {: class=info }
> The reference count of driver binary is normally to be decremented by calling [`driver_free_binary`](erl_driver.md#driver_free_binary).
>
> `driver_binary_dec_refc` does *not* free the binary if the reference count reaches zero. *Only* use `driver_binary_dec_refc` when you are sure *not* to reach a reference count of zero.

[](){: id=driver_binary_get_refc }
## driver_binary_get_refc/1

```c
long driver_binary_get_refc(ErlDrvBinary *bin);
```

[](){: id=driver_binary_get_refc }
Returns the current reference count on `bin`.

This function is thread-safe.

[](){: id=driver_binary_inc_refc }
## driver_binary_inc_refc/1

```c
long driver_binary_inc_refc(ErlDrvBinary *bin);
```

[](){: id=driver_binary_inc_refc }
Increments the reference count on `bin` and returns the reference count reached after the increment.

This function is thread-safe.

[](){: id=driver_caller }
## driver_caller/1

```c
ErlDrvTermData driver_caller(ErlDrvPort
        port);
```

[](){: id=driver_caller }
Returns the process ID of the process that made the current call to the driver. The process ID can be used with [`driver_send_term`](erl_driver.md#driver_send_term) to send back data to the caller. `driver_caller` only returns valid data when currently executing in one of the following driver callbacks:

* __[`start`](driver_entry.md#start)__ - Called from `erlang:open_port/2`.

* __[`output`](driver_entry.md#output)__ - Called from `erlang:send/2` and `erlang:port_command/2`.

* __[`outputv`](driver_entry.md#outputv)__ - Called from `erlang:send/2` and `erlang:port_command/2`.

* __[`control`](driver_entry.md#control)__ - Called from `erlang:port_control/3`.

* __[`call`](driver_entry.md#call)__ - Called from `erlang:port_call/3`.

Notice that this function is *not* thread-safe.

[](){: id=driver_cancel_timer }
## driver_cancel_timer/1

```c
int driver_cancel_timer(ErlDrvPort port);
```

[](){: id=driver_cancel_timer }
Cancels a timer set with [`driver_set_timer`](erl_driver.md#driver_set_timer).

The return value is `0`.

[](){: id=driver_compare_monitors }
## driver_compare_monitors/2

```c
int driver_compare_monitors(const ErlDrvMonitor
        *monitor1, const ErlDrvMonitor *monitor2);
```

[](){: id=driver_compare_monitors }
Compares two `ErlDrvMonitor`s. Can also be used to imply some artificial order on monitors, for whatever reason.

Returns `0` if `monitor1` and `monitor2` are equal, < `0` if `monitor1` < `monitor2`, and > `0` if `monitor1` > `monitor2`.

[](){: id=driver_connected }
## driver_connected/1

```c
ErlDrvTermData driver_connected(ErlDrvPort
        port);
```

[](){: id=driver_connected }
Returns the port owner process.

Notice that this function is *not* thread-safe.

[](){: id=driver_create_port }
## driver_create_port/4

```c
ErlDrvPort driver_create_port(ErlDrvPort port,
        ErlDrvTermData owner_pid, char* name,
        ErlDrvData drv_data);
```

Creates a new port executing the same driver code as the port creating the new port.

* __`port`__ - The port handle of the port (driver instance) creating the new port.

* __`owner_pid`__ - The process ID of the Erlang process to become owner of the new port. This process will be linked to the new port. You usually want to use `driver_caller(port)` as `owner_pid`.

* __`name`__ - The port name of the new port. You usually want to use the same port name as the driver name ([ `driver_name`](driver_entry.md#driver_name) field of the [`driver_entry`](driver_entry.md)).

* __`drv_data`__ - The driver-defined handle that is passed in later calls to driver callbacks. Notice that the [driver start callback](driver_entry.md#start) is not called for this new driver instance. The driver-defined handle is normally created in the [driver start callback](driver_entry.md#start) when a port is created through `erlang:open_port/2`.

The caller of `driver_create_port` is allowed to manipulate the newly created port when `driver_create_port` has returned. When [port level locking](erl_driver.md#smp_support) is used, the creating port is only allowed to manipulate the newly created port until the current driver callback, which was called by the emulator, returns.

[](){: id=driver_demonitor_process }
## driver_demonitor_process/2

```c
int driver_demonitor_process(ErlDrvPort port,
        const ErlDrvMonitor *monitor);
```

[](){: id=driver_demonitor_process }
Cancels a monitor created earlier.

Returns `0` if a monitor was removed and > 0 if the monitor no longer exists.

[](){: id=driver_deq }
## driver_deq/2

```c
ErlDrvSizeT driver_deq(ErlDrvPort port,
        ErlDrvSizeT size);
```

[](){: id=driver_deq }
Dequeues data by moving the head pointer forward in the driver queue by `size` bytes. The data in the queue is deallocated.

Returns the number of bytes remaining in the queue on success, otherwise `-1`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_enq }
## driver_enq/3

```c
int driver_enq(ErlDrvPort port, char* buf,
        ErlDrvSizeT len);
```

[](){: id=driver_enq }
Enqueues data in the driver queue. The data in `buf` is copied (`len` bytes) and placed at the end of the driver queue. The driver queue is normally used in a FIFO way.

The driver queue is available to queue output from the emulator to the driver (data from the driver to the emulator is queued by the emulator in normal Erlang message queues). This can be useful if the driver must wait for slow devices, and so on, and wants to yield back to the emulator. The driver queue is implemented as an `ErlIOVec`.

When the queue contains data, the driver does not close until the queue is empty.

The return value is `0`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_enq_bin }
## driver_enq_bin/4

```c
int driver_enq_bin(ErlDrvPort port,
        ErlDrvBinary *bin, ErlDrvSizeT offset, ErlDrvSizeT len);
```

[](){: id=driver_enq_bin }
Enqueues a driver binary in the driver queue. The data in `bin` at `offset` with length `len` is placed at the end of the queue. This function is most often faster than [`driver_enq`](erl_driver.md#driver_enq), because no data must be copied.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

The return value is `0`.

[](){: id=driver_enqv }
## driver_enqv/3

```c
int driver_enqv(ErlDrvPort port, ErlIOVec *ev,
        ErlDrvSizeT skip);
```

[](){: id=driver_enqv }
Enqueues the data in `ev`, skipping the first `skip` bytes of it, at the end of the driver queue. It is faster than [`driver_enq`](erl_driver.md#driver_enq), because no data must be copied.

The return value is `0`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_failure }
## driver_failure/2

[](){: id=driver_failure_atom }
## driver_failure_atom/2

[](){: id=driver_failure_posix }
## driver_failure_posix/2

```c
int driver_failure(ErlDrvPort port, int
        error);
```

```c
int driver_failure_atom(ErlDrvPort port, char
        *string);
```

```c
int driver_failure_posix(ErlDrvPort port, int
        error);
```

[](){: id=driver_failure_atom }
[](){: id=driver_failure_posix }
[](){: id=driver_failure }
Signals to Erlang that the driver has encountered an error and is to be closed. The port is closed and the tuple `{'EXIT', error, Err}` is sent to the port owner process, where error is an error atom (`driver_failure_atom` and `driver_failure_posix`) or an integer (`driver_failure`).

The driver is to fail only when in severe error situations, when the driver cannot possibly keep open, for example, buffer allocation gets out of memory. For normal errors it is more appropriate to send error codes with [`driver_output`](erl_driver.md#driver_output).

The return value is `0`.

[](){: id=driver_failure_eof }
## driver_failure_eof/1

```c
int driver_failure_eof(ErlDrvPort
        port);
```

[](){: id=driver_failure_eof }
Signals to Erlang that the driver has encountered an EOF and is to be closed, unless the port was opened with option `eof`, in which case `eof` is sent to the port. Otherwise the port is closed and an `'EXIT'` message is sent to the port owner process.

The return value is `0`.

[](){: id=driver_free }
## driver_free/1

```c
void driver_free(void *ptr);
```

[](){: id=driver_free }
Frees the memory pointed to by `ptr`. The memory is to have been allocated with `driver_alloc`. All allocated memory is to be deallocated, only once. There is no garbage collection in drivers.

This function is thread-safe.

[](){: id=driver_free_binary }
## driver_free_binary/1

```c
void driver_free_binary(ErlDrvBinary *bin);
```

[](){: id=driver_free_binary }
Frees a driver binary `bin`, allocated previously with [`driver_alloc_binary`](erl_driver.md#driver_alloc_binary). As binaries in Erlang are reference counted, the binary can still be around.

This function is thread-safe.

[](){: id=driver_get_monitored_process }
## driver_get_monitored_process/2

```c
ErlDrvTermData driver_get_monitored_process(ErlDrvPort port, const
        ErlDrvMonitor *monitor);
```

[](){: id=driver_get_monitored_process }
Returns the process ID associated with a living monitor. It can be used in the [`process_exit`](driver_entry.md#process_exit) callback to get the process identification for the exiting process.

Returns `driver_term_nil` if the monitor no longer exists.

[](){: id=driver_get_now }
## driver_get_now/1

```c
int driver_get_now(ErlDrvNowData *now);
```

[](){: id=driver_get_now }
> #### Warning {: class=warning }
> *This function is deprecated. Do not use it.* Use [`erl_drv_monotonic_time`](erl_driver.md#erl_drv_monotonic_time) (perhaps in combination with [`erl_drv_time_offset`](erl_driver.md#erl_drv_time_offset)) instead.

Reads a time stamp into the memory pointed to by parameter `now`. For information about specific fields, see [`ErlDrvNowData`](erl_driver.md#erldrvnowdata).

The return value is `0`, unless the `now` pointer is invalid, in which case it is < `0`.

[](){: id=driver_lock_driver }
## driver_lock_driver/1

```c
int driver_lock_driver(ErlDrvPort
        port);
```

[](){: id=driver_lock_driver }
Locks the driver used by the port `port` in memory for the rest of the emulator process' lifetime. After this call, the driver behaves as one of Erlang's statically linked-in drivers.

[](){: id=driver_mk_atom }
## driver_mk_atom/1

```c
ErlDrvTermData driver_mk_atom(char*
        string);
```

[](){: id=driver_mk_atom }
Returns an atom given a name `string`. The atom is created and does not change, so the return value can be saved and reused, which is faster than looking up the atom several times.

Notice that this function is *not* thread-safe.

[](){: id=driver_mk_port }
## driver_mk_port/1

```c
ErlDrvTermData driver_mk_port(ErlDrvPort
        port);
```

[](){: id=driver_mk_port }
Converts a port handle to the Erlang term format, usable in [`erl_drv_output_term`](erl_driver.md#erl_drv_output_term) and [`erl_drv_send_term`](erl_driver.md#erl_drv_send_term).

Notice that this function is *not* thread-safe.

[](){: id=driver_monitor_process }
## driver_monitor_process/3

```c
int driver_monitor_process(ErlDrvPort port,
        ErlDrvTermData process, ErlDrvMonitor *monitor);
```

[](){: id=driver_monitor_process }
Starts monitoring a process from a driver. When a process is monitored, a process exit results in a call to the provided [`process_exit`](driver_entry.md#process_exit) callback in the [`ErlDrvEntry`](driver_entry.md) structure. The `ErlDrvMonitor` structure is filled in, for later removal or compare.

Parameter `process` is to be the return value of an earlier call to [`driver_caller`](erl_driver.md#driver_caller) or [`driver_connected`](erl_driver.md#driver_connected) call.

Returns `0` on success, < 0 if no callback is provided, and > 0 if the process is no longer alive.

[](){: id=driver_output }
## driver_output/3

```c
int driver_output(ErlDrvPort port, char *buf,
        ErlDrvSizeT len);
```

[](){: id=driver_output }
Sends data from the driver up to the emulator. The data is received as terms or binary data, depending on how the driver port was opened.

The data is queued in the port owner process' message queue. Notice that this does not yield to the emulator (as the driver and the emulator run in the same thread).

Parameter `buf` points to the data to send, and `len` is the number of bytes.

The return value for all output functions is `0` for normal use. If the driver is used for distribution, it can fail and return `-1`.

[](){: id=driver_output_binary }
## driver_output_binary/6

```c
int driver_output_binary(ErlDrvPort port, char
        *hbuf, ErlDrvSizeT hlen, ErlDrvBinary* bin, ErlDrvSizeT offset,
        ErlDrvSizeT len);
```

[](){: id=driver_output_binary }
Sends data to a port owner process from a driver binary. It has a header buffer (`hbuf` and `hlen`) just like [`driver_output2`](erl_driver.md#driver_output2). Parameter `hbuf` can be `NULL`.

Parameter `offset` is an offset into the binary and `len` is the number of bytes to send.

Driver binaries are created with [`driver_alloc_binary`](erl_driver.md#driver_alloc_binary).

The data in the header is sent as a list and the binary as an Erlang binary in the tail of the list.

For example, if `hlen` is `2`, the port owner process receives `[H1, H2 | <<T>>]`.

The return value is `0` for normal use.

Notice that, using the binary syntax in Erlang, the driver application can match the header directly from the binary, so the header can be put in the binary, and `hlen` can be set to `0`.

[](){: id=driver_output_term }
## driver_output_term/3

```c
int driver_output_term(ErlDrvPort port,
        ErlDrvTermData* term, int n);
```

[](){: id=driver_output_term }
> #### Warning {: class=warning }
> *This function is deprecated.* Use [`erl_drv_output_term`](erl_driver.md#erl_drv_send_term)instead.

Parameters `term` and `n` work as in [`erl_drv_output_term`](erl_driver.md#erl_drv_output_term).

Notice that this function is *not* thread-safe.

[](){: id=driver_output2 }
## driver_output2/5

```c
int driver_output2(ErlDrvPort port, char *hbuf,
        ErlDrvSizeT hlen, char *buf, ErlDrvSizeT len);
```

[](){: id=driver_output2 }
First sends `hbuf` (length in `hlen`) data as a list, regardless of port settings. Then sends `buf` as a binary or list. For example, if `hlen` is `3`, the port owner process receives `[H1, H2, H3 | T]`.

The point of sending data as a list header, is to facilitate matching on the data received.

The return value is `0` for normal use.

[](){: id=driver_outputv }
## driver_outputv/5

```c
int driver_outputv(ErlDrvPort port, char* hbuf,
        ErlDrvSizeT hlen, ErlIOVec *ev, ErlDrvSizeT skip);
```

[](){: id=driver_outputv }
Sends data from an I/O vector, `ev`, to the port owner process. It has a header buffer (`hbuf` and `hlen`), just like [`driver_output2`](erl_driver.md#driver_output2).

Parameter `skip` is a number of bytes to skip of the `ev` vector from the head.

You get vectors of `ErlIOVec` type from the driver queue (see below), and the [`outputv`](driver_entry.md#outputv) driver entry function. You can also make them yourself, if you want to send several `ErlDrvBinary` buffers at once. Often it is faster to use [`driver_output`](erl_driver.md#driver_output) or .

For example, if `hlen` is `2` and `ev` points to an array of three binaries, the port owner process receives `[H1, H2, <<B1>>, <<B2>> | <<B3>>]`.

The return value is `0` for normal use.

The comment for `driver_output_binary` also applies for `driver_outputv`.

[](){: id=driver_pdl_create }
## driver_pdl_create/1

```c
ErlDrvPDL driver_pdl_create(ErlDrvPort port);
```

[](){: id=driver_pdl_create }
Creates a port data lock associated with the `port`.

> #### Note {: class=info }
> Once a port data lock has been created, it must be locked during all operations on the driver queue of the `port`.

Returns a newly created port data lock on success, otherwise `NULL`. The function fails if `port` is invalid or if a port data lock already has been associated with the `port`.

[](){: id=driver_pdl_dec_refc }
## driver_pdl_dec_refc/1

```c
long driver_pdl_dec_refc(ErlDrvPDL
        pdl);
```

[](){: id=driver_pdl_dec_refc }
Decrements the reference count of the port data lock passed as argument (`pdl`).

The current reference count after the decrement has been performed is returned.

This function is thread-safe.

[](){: id=driver_pdl_get_refc }
## driver_pdl_get_refc/1

```c
long driver_pdl_get_refc(ErlDrvPDL pdl);
```

[](){: id=driver_pdl_get_refc }
Returns the current reference count of the port data lock passed as argument (`pdl`).

This function is thread-safe.

[](){: id=driver_pdl_inc_refc }
## driver_pdl_inc_refc/1

```c
long driver_pdl_inc_refc(ErlDrvPDL pdl);
```

[](){: id=driver_pdl_inc_refc }
Increments the reference count of the port data lock passed as argument (`pdl`).

The current reference count after the increment has been performed is returned.

This function is thread-safe.

[](){: id=driver_pdl_lock }
## driver_pdl_lock/1

```c
void driver_pdl_lock(ErlDrvPDL pdl);
```

[](){: id=driver_pdl_lock }
Locks the port data lock passed as argument (`pdl`).

This function is thread-safe.

[](){: id=driver_pdl_unlock }
## driver_pdl_unlock/1

```c
void driver_pdl_unlock(ErlDrvPDL pdl);
```

[](){: id=driver_pdl_unlock }
Unlocks the port data lock passed as argument (`pdl`).

This function is thread-safe.

[](){: id=driver_peekq }
## driver_peekq/2

```c
SysIOVec * driver_peekq(ErlDrvPort port, int
        *vlen);
```

[](){: id=driver_peekq }
Retrieves the driver queue as a pointer to an array of `SysIOVec`s. It also returns the number of elements in `vlen`. This is one of two ways to get data out of the queue.

Nothing is removed from the queue by this function, that must be done with [`driver_deq`](erl_driver.md#driver_deq).

The returned array is suitable to use with the Unix system call `writev`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_peekqv }
## driver_peekqv/2

```c
ErlDrvSizeT driver_peekqv(ErlDrvPort port,
        ErlIOVec *ev);
```

[](){: id=driver_peekqv }
Retrieves the driver queue into a supplied `ErlIOVec` `ev`. It also returns the queue size. This is one of two ways to get data out of the queue.

If `ev` is `NULL`, all ones that is `-1` type cast to `ErlDrvSizeT` are returned.

Nothing is removed from the queue by this function, that must be done with [`driver_deq`](erl_driver.md#driver_deq).

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_pushq }
## driver_pushq/3

```c
int driver_pushq(ErlDrvPort port, char* buf,
        ErlDrvSizeT len);
```

[](){: id=driver_pushq }
Puts data at the head of the driver queue. The data in `buf` is copied (`len` bytes) and placed at the beginning of the queue.

The return value is `0`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_pushq_bin }
## driver_pushq_bin/4

```c
int driver_pushq_bin(ErlDrvPort port,
        ErlDrvBinary *bin, ErlDrvSizeT offset, ErlDrvSizeT len);
```

[](){: id=driver_pushq_bin }
Puts data in the binary `bin`, at `offset` with length `len` at the head of the driver queue. It is most often faster than [`driver_pushq`](erl_driver.md#driver_pushq), because no data must be copied.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

The return value is `0`.

[](){: id=driver_pushqv }
## driver_pushqv/3

```c
int driver_pushqv(ErlDrvPort port, ErlIOVec
        *ev, ErlDrvSizeT skip);
```

[](){: id=driver_pushqv }
Puts the data in `ev`, skipping the first `skip` bytes of it, at the head of the driver queue. It is faster than [`driver_pushq`](erl_driver.md#driver_pushq), because no data must be copied.

The return value is `0`.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_read_timer }
## driver_read_timer/2

```c
int driver_read_timer(ErlDrvPort port, unsigned
        long *time_left);
```

[](){: id=driver_read_timer }
Reads the current time of a timer, and places the result in `time_left`. This is the time in milliseconds, before the time-out occurs.

The return value is `0`.

[](){: id=driver_realloc }
## driver_realloc/2

```c
void * driver_realloc(void *ptr, ErlDrvSizeT size);
```

[](){: id=driver_realloc }
Resizes a memory block, either in place, or by allocating a new block, copying the data, and freeing the old block. A pointer is returned to the reallocated memory. On failure (out of memory), `NULL` is returned. (This is most often a wrapper for `realloc`.)

This function is thread-safe.

[](){: id=driver_realloc_binary }
## driver_realloc_binary/2

```c
ErlDrvBinary * driver_realloc_binary(ErlDrvBinary *bin, ErlDrvSizeT size);
```

[](){: id=driver_realloc_binary }
Resizes a driver binary, while keeping the data.

Returns the resized driver binary on success. Returns `NULL` on failure (out of memory).

This function is thread-safe.

[](){: id=driver_select }
## driver_select/4

```c
int driver_select(ErlDrvPort port, ErlDrvEvent
        event, int mode, int on);
```

[](){: id=driver_select }
This function is used by drivers to provide the emulator with events to check for. This enables the emulator to call the driver when something has occurred asynchronously.

Parameter `event` identifies an OS-specific event object. On Unix systems, the functions `select`/`poll` are used. The event object must be a socket or pipe (or other object that `select`/`poll` can use). On Windows, the Win32 API function `WaitForMultipleObjects` is used. This places other restrictions on the event object; see the Win32 SDK documentation.

Parameter `on` is to be `1` for setting events and `0` for clearing them.

Parameter `mode` is a bitwise OR combination of `ERL_DRV_READ`, `ERL_DRV_WRITE`, and `ERL_DRV_USE`. The first two specify whether to wait for read events and/or write events. A fired read event calls [`ready_input`](driver_entry.md#ready_input) and a fired write event calls [`ready_output`](driver_entry.md#ready_output).

> #### Note {: class=info }
> Some OS (Windows) do not differentiate between read and write events. The callback for a fired event then only depends on the value of `mode`.

`ERL_DRV_USE` specifies if we are using the event object or if we want to close it. It is not safe to clear all events and then close the event object after `driver_select` has returned. Another thread can still be using the event object internally. To safely close an event object, call `driver_select` with `ERL_DRV_USE` and `on==0`, which clears all events and then either calls [`stop_select`](driver_entry.md#stop_select) or schedules it to be called when it is safe to close the event object. `ERL_DRV_USE` is to be set together with the first event for an event object. It is harmless to set `ERL_DRV_USE` even if it already has been done. Clearing all events but keeping `ERL_DRV_USE` set indicates that we are using the event object and probably will set events for it again.

> #### Note {: class=info }
> `ERL_DRV_USE` was added in Erlang/OTP R13. Old drivers still work as before, but it is recommended to update them to use `ERL_DRV_USE` and `stop_select` to ensure that event objects are closed in a safe way.

The return value is `0`, unless `ready_input`/`ready_output` is `NULL`, in which case it is `-1`.

[](){: id=driver_send_term }
## driver_send_term/4

```c
int driver_send_term(ErlDrvPort port,
        ErlDrvTermData receiver, ErlDrvTermData* term, int n);
```

[](){: id=driver_send_term }
> #### Warning {: class=warning }
> *This function is deprecated.* Use [`erl_drv_send_term`](erl_driver.md#erl_drv_send_term) instead.

> #### Note {: class=info }
> The parameters of this function cannot be properly checked by the runtime system when executed by arbitrary threads. This can cause the function not to fail when it should.

Parameters `term` and `n` work as in [`erl_drv_output_term`](erl_driver.md#erl_drv_output_term).

This function is thread-safe.

[](){: id=driver_set_timer }
## driver_set_timer/2

```c
int driver_set_timer(ErlDrvPort port, unsigned
        long time);
```

[](){: id=driver_set_timer }
Sets a timer on the driver, which will count down and call the driver when it is timed out. Parameter `time` is the time in milliseconds before the timer expires.

When the timer reaches `0` and expires, the driver entry function [`timeout`](driver_entry.md#timeout) is called.

Notice that only one timer exists on each driver instance; setting a new timer replaces an older one.

Return value is `0`, unless the `timeout` driver function is `NULL`, in which case it is `-1`.

[](){: id=driver_sizeq }
## driver_sizeq/1

```c
ErlDrvSizeT driver_sizeq(ErlDrvPort port);
```

[](){: id=driver_sizeq }
Returns the number of bytes currently in the driver queue.

This function can be called from any thread if a [port data lock](erl_driver.md#erldrvpdl) associated with the `port` is locked by the calling thread during the call.

[](){: id=driver_system_info }
## driver_system_info/2

```c
void driver_system_info(ErlDrvSysInfo
        *sys_info_ptr, size_t size);
```

[](){: id=driver_system_info }
Writes information about the Erlang runtime system into the [`ErlDrvSysInfo`](erl_driver.md#erldrvsysinfo) structure referred to by the first argument. The second argument is to be the size of the [`ErlDrvSysInfo`](erl_driver.md#erldrvsysinfo) structure, that is, `sizeof(ErlDrvSysInfo)`.

For information about specific fields, see [`ErlDrvSysInfo`](erl_driver.md#erldrvsysinfo).

[](){: id=driver_vec_to_buf }
## driver_vec_to_buf/3

```c
ErlDrvSizeT driver_vec_to_buf(ErlIOVec *ev,
        char *buf, ErlDrvSizeT len);
```

[](){: id=driver_vec_to_buf }
Collects several segments of data, referenced by `ev`, by copying them in order to the buffer `buf`, of the size `len`.

If the data is to be sent from the driver to the port owner process, it is faster to use [`driver_outputv`](erl_driver.md#driver_outputv).

The return value is the space left in the buffer, that is, if `ev` contains less than `len` bytes it is the difference, and if `ev` contains `len` bytes or more, it is `0`. This is faster if there is more than one header byte, as the binary syntax can construct integers directly from the binary.

[](){: id=erl_drv_busy_msgq_limits }
## erl_drv_busy_msgq_limits/3

```c
void erl_drv_busy_msgq_limits(ErlDrvPort port,
        ErlDrvSizeT *low, ErlDrvSizeT *high);
```

[](){: id=erl_drv_busy_msgq_limits }
Sets and gets limits that will be used for controlling the busy state of the port message queue.

The port message queue is set into a busy state when the amount of command data queued on the message queue reaches the `high` limit. The port message queue is set into a not busy state when the amount of command data queued on the message queue falls below the `low` limit. Command data is in this context data passed to the port using either `Port ! {Owner, {command, Data}}` or `port_command/[2,3]`. Notice that these limits only concerns command data that have not yet reached the port. The [busy port](erl_driver.md#set_busy_port) feature can be used for data that has reached the port.

Valid limits are values in the range `[ERL_DRV_BUSY_MSGQ_LIM_MIN, ERL_DRV_BUSY_MSGQ_LIM_MAX]`. Limits are automatically adjusted to be sane. That is, the system adjusts values so that the low limit used is lower than or equal to the high limit used. By default the high limit is 8 kB and the low limit is 4 kB.

By passing a pointer to an integer variable containing the value `ERL_DRV_BUSY_MSGQ_READ_ONLY`, the currently used limit is read and written back to the integer variable. A new limit can be set by passing a pointer to an integer variable containing a valid limit. The passed value is written to the internal limit. The internal limit is then adjusted. After this the adjusted limit is written back to the integer variable from which the new value was read. Values are in bytes.

The busy message queue feature can be disabled either by setting the `ERL_DRV_FLAG_NO_BUSY_MSGQ` [driver flag](driver_entry.md#driver_flags) in the [`driver_entry`](driver_entry.md) used by the driver, or by calling this function with `ERL_DRV_BUSY_MSGQ_DISABLED` as a limit (either low or high). When this feature has been disabled, it cannot be enabled again. When reading the limits, both are `ERL_DRV_BUSY_MSGQ_DISABLED` if this feature has been disabled.

Processes sending command data to the port are suspended if either the port is busy or if the port message queue is busy. Suspended processes are resumed when neither the port or the port message queue is busy.

For information about busy port functionality, see [`set_busy_port`](erl_driver.md#set_busy_port).

[](){: id=erl_drv_cond_broadcast }
## erl_drv_cond_broadcast/1

```c
void erl_drv_cond_broadcast(ErlDrvCond
        *cnd);
```

[](){: id=erl_drv_cond_broadcast }
Broadcasts on a condition variable. That is, if other threads are waiting on the condition variable being broadcast on, *all* of them are woken.

`cnd` is a pointer to a condition variable to broadcast on.

This function is thread-safe.

[](){: id=erl_drv_cond_create }
## erl_drv_cond_create/1

```c
ErlDrvCond * erl_drv_cond_create(char
        *name);
```

[](){: id=erl_drv_cond_create }
Creates a condition variable and returns a pointer to it.

`name` is a string identifying the created condition variable. It is used to identify the condition variable in planned future debug functionality.

Returns `NULL` on failure. The driver creating the condition variable is responsible for destroying it before the driver is unloaded.

This function is thread-safe.

[](){: id=erl_drv_cond_destroy }
## erl_drv_cond_destroy/1

```c
void erl_drv_cond_destroy(ErlDrvCond
        *cnd);
```

[](){: id=erl_drv_cond_destroy }
Destroys a condition variable previously created by [`erl_drv_cond_create`](erl_driver.md#erl_drv_cond_create).

`cnd` is a pointer to a condition variable to destroy.

This function is thread-safe.

[](){: id=erl_drv_cond_name }
## erl_drv_cond_name/1

```c
char * erl_drv_cond_name(ErlDrvCond
        *cnd);
```

[](){: id=erl_drv_cnd_name }
Returns a pointer to the name of the condition.

`cnd` is a pointer to an initialized condition.

> #### Note {: class=info }
> This function is intended for debugging purposes only.

[](){: id=erl_drv_cond_signal }
## erl_drv_cond_signal/1

```c
void erl_drv_cond_signal(ErlDrvCond
        *cnd);
```

[](){: id=erl_drv_cond_signal }
Signals on a condition variable. That is, if other threads are waiting on the condition variable being signaled, *one* of them is woken.

`cnd` is a pointer to a condition variable to signal on.

This function is thread-safe.

[](){: id=erl_drv_cond_wait }
## erl_drv_cond_wait/2

```c
void erl_drv_cond_wait(ErlDrvCond *cnd,
        ErlDrvMutex *mtx);
```

[](){: id=erl_drv_cond_wait }
Waits on a condition variable. The calling thread is blocked until another thread wakes it by signaling or broadcasting on the condition variable. Before the calling thread is blocked, it unlocks the mutex passed as argument. When the calling thread is woken, it locks the same mutex before returning. That is, the mutex currently must be locked by the calling thread when calling this function.

`cnd` is a pointer to a condition variable to wait on. `mtx` is a pointer to a mutex to unlock while waiting.

> #### Note {: class=info }
> `erl_drv_cond_wait` can return even if no one has signaled or broadcast on the condition variable. Code calling `erl_drv_cond_wait` is always to be prepared for `erl_drv_cond_wait` returning even if the condition that the thread was waiting for has not occurred. That is, when returning from `erl_drv_cond_wait`, always check if the condition has occurred, and if not call `erl_drv_cond_wait` again.

This function is thread-safe.

[](){: id=erl_drv_consume_timeslice }
## erl_drv_consume_timeslice/2

```c
int erl_drv_consume_timeslice(ErlDrvPort port,
        int percent);
```

[](){: id=erl_drv_consume_timeslice }
Gives the runtime system a hint about how much CPU time the current driver callback call has consumed since the last hint, or since the the start of the callback if no previous hint has been given.

* __`port`__ - Port handle of the executing port.

* __`percent`__ - Approximate consumed fraction of a full time-slice in percent.

The time is specified as a fraction, in percent, of a full time-slice that a port is allowed to execute before it is to surrender the CPU to other runnable ports or processes. Valid range is `[1, 100]`. The scheduling time-slice is not an exact entity, but can usually be approximated to about 1 millisecond.

Notice that it is up to the runtime system to determine if and how to use this information. Implementations on some platforms can use other means to determine the consumed fraction of the time-slice. Lengthy driver callbacks should, regardless of this, frequently call this function to determine if it is allowed to continue execution or not.

This function returns a non-zero value if the time-slice has been exhausted, and zero if the callback is allowed to continue execution. If a non-zero value is returned, the driver callback is to return as soon as possible in order for the port to be able to yield.

This function is provided to better support co-operative scheduling, improve system responsiveness, and to make it easier to prevent misbehaviors of the VM because of a port monopolizing a scheduler thread. It can be used when dividing lengthy work into some repeated driver callback calls, without the need to use threads.

See also the important [warning](erl_driver.md#warning) text at the beginning of this manual page.

[](){: id=erl_drv_convert_time_unit }
## erl_drv_convert_time_unit/3

```c
ErlDrvTime erl_drv_convert_time_unit(ErlDrvTime
        val, ErlDrvTimeUnit from, ErlDrvTimeUnit to);
```

[](){: id=erl_drv_convert_time_unit }
Converts the `val` value of time unit `from` to the corresponding value of time unit `to`. The result is rounded using the floor function.

* __`val`__ - Value to convert time unit for.

* __`from`__ - Time unit of `val`.

* __`to`__ - Time unit of returned value.

Returns `ERL_DRV_TIME_ERROR` if called with an invalid time unit argument.

See also [`ErlDrvTime`](erl_driver.md#erldrvtime) and [`ErlDrvTimeUnit`](erl_driver.md#erldrvtimeunit).

[](){: id=erl_drv_equal_tids }
## erl_drv_equal_tids/2

```c
int erl_drv_equal_tids(ErlDrvTid tid1,
        ErlDrvTid tid2);
```

[](){: id=erl_drv_equal_tids }
Compares two thread identifiers, `tid1` and `tid2`, for equality.

Returns `0` it they are not equal, and a value not equal to `0` if they are equal.

> #### Note {: class=info }
> A thread identifier can be reused very quickly after a thread has terminated. Therefore, if a thread corresponding to one of the involved thread identifiers has terminated since the thread identifier was saved, the result of `erl_drv_equal_tids` does possibly not give the expected result.

This function is thread-safe.

[](){: id=erl_drv_getenv }
## erl_drv_getenv/3

```c
int erl_drv_getenv(const char *key, char
        *value, size_t *value_size);
```

[](){: id=erl_drv_getenv }
Retrieves the value of an environment variable.

* __`key`__ - A `NULL`\-terminated string containing the name of the environment variable.

* __`value`__ - A pointer to an output buffer.

* __`value_size`__ - A pointer to an integer. The integer is used both for passing input and output sizes (see below).

When this function is called, `*value_size` is to contain the size of the `value` buffer.

On success, `0` is returned, the value of the environment variable has been written to the `value` buffer, and `*value_size` contains the string length (excluding the terminating `NULL` character) of the value written to the `value` buffer.

On failure, that is, no such environment variable was found, a value < `0` is returned. When the size of the `value` buffer is too small, a value > `0` is returned and `*value_size` has been set to the buffer size needed.

> #### Warning {: class=warning }
> This function reads the emulated environment used by `os:getenv/1` and not the environment used by libc's `m:getenv` or similar. Drivers that *require* that these are in sync will need to do so themselves, but keep in mind that they are segregated for a reason; `m:getenv` and its friends are *not thread-safe* and may cause unrelated code to misbehave or crash the emulator.

This function is thread-safe.

[](){: id=erl_drv_init_ack }
## erl_drv_init_ack/2

```c
void erl_drv_init_ack(ErlDrvPort port,
        ErlDrvData res);
```

[](){: id=erl_drv_init_ack }
Acknowledges the start of the port.

* __`port`__ - The port handle of the port (driver instance) doing the acknowledgment.

* __`res`__ - The result of the port initialization. Can be the same values as the return value of [`start`](driver_entry.md#start), that is, any of the error codes or the `ErlDrvData` that is to be used for this port.

When this function is called the initiating `erlang:open_port` call is returned as if the [`start`](driver_entry.md#start) function had just been called. It can only be used when flag [`ERL_DRV_FLAG_USE_INIT_ACK`](driver_entry.md#driver_flags) has been set on the linked-in driver.

[](){: id=erl_drv_monotonic_time }
## erl_drv_monotonic_time/1

```c
ErlDrvTime erl_drv_monotonic_time(ErlDrvTimeUnit time_unit);
```

[](){: id=erl_drv_monotonic_time }
Returns [Erlang monotonic time](time_correction.md#erlang_monotonic_time). Notice that negative values are not uncommon.

`time_unit` is time unit of returned value.

Returns `ERL_DRV_TIME_ERROR` if called with an invalid time unit argument, or if called from a thread that is not a scheduler thread.

See also [`ErlDrvTime`](erl_driver.md#erldrvtime) and [`ErlDrvTimeUnit`](erl_driver.md#erldrvtimeunit).

[](){: id=erl_drv_mutex_create }
## erl_drv_mutex_create/1

```c
ErlDrvMutex * erl_drv_mutex_create(char
        *name);
```

[](){: id=erl_drv_mutex_create }
Creates a mutex and returns a pointer to it.

`name` is a string identifying the created mutex. It is used to identify the mutex in debug functionality (see note).

Returns `NULL` on failure. The driver creating the mutex is responsible for destroying it before the driver is unloaded.

This function is thread-safe.

[](){: id=lock_checker }
> #### Note {: class=info }
> One such debug functionality is the *lock checker*, which can detect locking order violations and thereby potential deadlock bugs. For the lock checker to work the `name` should be on the format `"App.Type"` or `"App.Type[Instance]"`, where App is the name of the application, Type is the name of the lock type and Instance is optional information about each lock instance. "App.Type" should be a unique name for the lock checker to detect lock order violations between locks of different types. The Instance information is currently ignored.
>
> For example, if we have mutexes of types "myapp.xtable" and "myapp.xitem" then the lock checker will make sure either "myapp.xtable" locks are never locked after "myapp.xitem" locks or vice versa.

[](){: id=erl_drv_mutex_destroy }
## erl_drv_mutex_destroy/1

```c
void erl_drv_mutex_destroy(ErlDrvMutex
        *mtx);
```

[](){: id=erl_drv_mutex_destroy }
Destroys a mutex previously created by [`erl_drv_mutex_create`](erl_driver.md#erl_drv_mutex_create). The mutex must be in an unlocked state before it is destroyed.

`mtx` is a pointer to a mutex to destroy.

This function is thread-safe.

[](){: id=erl_drv_mutex_lock }
## erl_drv_mutex_lock/1

```c
void erl_drv_mutex_lock(ErlDrvMutex
        *mtx);
```

[](){: id=erl_drv_mutex_lock }
Locks a mutex. The calling thread is blocked until the mutex has been locked. A thread that has currently locked the mutex *cannot* lock the same mutex again.

`mtx` is a pointer to a mutex to lock.

> #### Warning {: class=warning }
> If you leave a mutex locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_mutex_name }
## erl_drv_mutex_name/1

```c
char * erl_drv_mutex_name(ErlDrvMutex
        *mtx);
```

[](){: id=erl_drv_mutex_name }
Returns a pointer to the mutex name.

`mtx` is a pointer to an initialized mutex.

> #### Note {: class=info }
> This function is intended for debugging purposes only.

[](){: id=erl_drv_mutex_trylock }
## erl_drv_mutex_trylock/1

```c
int erl_drv_mutex_trylock(ErlDrvMutex
        *mtx);
```

[](){: id=erl_drv_mutex_trylock }
Tries to lock a mutex. A thread that has currently locked the mutex *cannot* try to lock the same mutex again.

`mtx` is a pointer to a mutex to try to lock.

Returns `0` on success, otherwise `EBUSY`.

> #### Warning {: class=warning }
> If you leave a mutex locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_mutex_unlock }
## erl_drv_mutex_unlock/1

```c
void erl_drv_mutex_unlock(ErlDrvMutex
        *mtx);
```

[](){: id=erl_drv_mutex_unlock }
Unlocks a mutex. The mutex currently must be locked by the calling thread.

`mtx` is a pointer to a mutex to unlock.

This function is thread-safe.

[](){: id=erl_drv_output_term }
## erl_drv_output_term/3

```c
int erl_drv_output_term(ErlDrvTermData port,
        ErlDrvTermData* term, int n);
```

[](){: id=erl_drv_output_term }
Sends data in the special driver term format to the port owner process. This is a fast way to deliver term data from a driver. It needs no binary conversion, so the port owner process receives data as normal Erlang terms. The [`erl_drv_send_term`](erl_driver.md#erl_drv_send_term) functions can be used for sending to any process on the local node.

> #### Note {: class=info }
> Parameter `port` is *not* an ordinary port handle, but a port handle converted using [`driver_mk_port`](erl_driver.md#driver_mk_port).

Parameter `term` points to an array of `ErlDrvTermData` with `n` elements. This array contains terms described in the driver term format. Every term consists of 1-4 elements in the array. The first term has a term type and then arguments. Parameter `port` specifies the sending port.

Tuples, maps, and lists (except strings, see below) are built in reverse polish notation, so that to build a tuple, the elements are specified first, and then the tuple term, with a count. Likewise for lists and maps.

* A tuple must be specified with the number of elements. (The elements precede the `ERL_DRV_TUPLE` term.)
* A map must be specified with the number of key-value pairs `N`. The key-value pairs must precede the `ERL_DRV_MAP` in this order: `key1,value1,key2,value2,...,keyN,valueN`. Duplicate keys are not allowed.
* A list must be specified with the number of elements, including the tail, which is the last term preceding `ERL_DRV_LIST`.

The special term `ERL_DRV_STRING_CONS` is used to "splice" in a string in a list, a string specified this way is not a list in itself, but the elements are elements of the surrounding list.

```text
Term type            Arguments
---------            ---------
ERL_DRV_NIL
ERL_DRV_ATOM         ErlDrvTermData atom (from driver_mk_atom(char *string))
ERL_DRV_INT          ErlDrvSInt integer
ERL_DRV_UINT         ErlDrvUInt integer
ERL_DRV_INT64        ErlDrvSInt64 *integer_ptr
ERL_DRV_UINT64       ErlDrvUInt64 *integer_ptr
ERL_DRV_PORT         ErlDrvTermData port (from driver_mk_port(ErlDrvPort port))
ERL_DRV_BINARY       ErlDrvBinary *bin, ErlDrvUInt len, ErlDrvUInt offset
ERL_DRV_BUF2BINARY   char *buf, ErlDrvUInt len
ERL_DRV_STRING       char *str, int len
ERL_DRV_TUPLE        int sz
ERL_DRV_LIST         int sz
ERL_DRV_PID          ErlDrvTermData pid (from driver_connected(ErlDrvPort port)
                     or driver_caller(ErlDrvPort port))
ERL_DRV_STRING_CONS  char *str, int len
ERL_DRV_FLOAT        double *dbl
ERL_DRV_EXT2TERM     char *buf, ErlDrvUInt len
ERL_DRV_MAP          int sz
```

The unsigned integer data type `ErlDrvUInt` and the signed integer data type `ErlDrvSInt` are 64 bits wide on a 64-bit runtime system and 32 bits wide on a 32-bit runtime system. They were introduced in ERTS 5.6 and replaced some of the `int` arguments in the list above.

The unsigned integer data type `ErlDrvUInt64` and the signed integer data type `ErlDrvSInt64` are always 64 bits wide. They were introduced in ERTS 5.7.4.

To build the tuple `{tcp, Port, [100 | Binary]}`, the following call can be made.

```text
ErlDrvBinary* bin = ...
ErlDrvPort port = ...
ErlDrvTermData spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("tcp"),
    ERL_DRV_PORT, driver_mk_port(drvport),
        ERL_DRV_INT, 100,
        ERL_DRV_BINARY, bin, 50, 0,
        ERL_DRV_LIST, 2,
    ERL_DRV_TUPLE, 3,
};
erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));
```

Here `bin` is a driver binary of length at least 50 and `drvport` is a port handle. Notice that `ERL_DRV_LIST` comes after the elements of the list, likewise `ERL_DRV_TUPLE`.

The `ERL_DRV_STRING_CONS` term is a way to construct strings. It works differently from how `ERL_DRV_STRING` works. `ERL_DRV_STRING_CONS` builds a string list in reverse order (as opposed to how `ERL_DRV_LIST` works), concatenating the strings added to a list. The tail must be specified before `ERL_DRV_STRING_CONS`.

`ERL_DRV_STRING` constructs a string, and ends it. (So it is the same as `ERL_DRV_NIL` followed by `ERL_DRV_STRING_CONS`.)

```text
/* to send [x, "abc", y] to the port: */
ErlDrvTermData spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("x"),
    ERL_DRV_STRING, (ErlDrvTermData)"abc", 3,
    ERL_DRV_ATOM, driver_mk_atom("y"),
    ERL_DRV_NIL,
    ERL_DRV_LIST, 4
};
erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));
```

```text
/* to send "abc123" to the port: */
ErlDrvTermData spec[] = {
    ERL_DRV_NIL,        /* with STRING_CONS, the tail comes first */
    ERL_DRV_STRING_CONS, (ErlDrvTermData)"123", 3,
    ERL_DRV_STRING_CONS, (ErlDrvTermData)"abc", 3,
};
erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));
```

[](){: id=ERL_DRV_EXT2TERM }
The `ERL_DRV_EXT2TERM` term type is used for passing a term encoded with the [external format](erl_ext_dist.md), that is, a term that has been encoded by [`erlang:term_to_binary()`](`erlang:term_to_binary/2`), [`erl_interface:ei(3)`](`p:erl_interface:ei.md`), and so on. For example, if `binp` is a pointer to an `ErlDrvBinary` that contains term `{17, 4711}` encoded with the [external format](erl_ext_dist.md), and you want to wrap it in a two-tuple with the tag `my_tag`, that is, `{my_tag, {17, 4711}}`, you can do as follows:

```text
ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("my_tag"),
        ERL_DRV_EXT2TERM, (ErlDrvTermData) binp->orig_bytes, binp->orig_size
    ERL_DRV_TUPLE, 2,
};
erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));
```

To build the map `#{key1 => 100, key2 => {200, 300}}`, the following call can be made.

```text
ErlDrvPort port = ...
ErlDrvTermData spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("key1"),
        ERL_DRV_INT, 100,
    ERL_DRV_ATOM, driver_mk_atom("key2"),
        ERL_DRV_INT, 200,
        ERL_DRV_INT, 300,
    ERL_DRV_TUPLE, 2,
    ERL_DRV_MAP, 2
};
erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));
```

If you want to pass a binary and do not already have the content of the binary in an `ErlDrvBinary`, you can benefit from using `ERL_DRV_BUF2BINARY` instead of creating an `ErlDrvBinary` through [`driver_alloc_binary`](erl_driver.md#driver_alloc_binary) and then pass the binary through `ERL_DRV_BINARY`. The runtime system often allocates binaries smarter if `ERL_DRV_BUF2BINARY` is used. However, if the content of the binary to pass already resides in an `ErlDrvBinary`, it is normally better to pass the binary using `ERL_DRV_BINARY` and the `ErlDrvBinary` in question.

The `ERL_DRV_UINT`, `ERL_DRV_BUF2BINARY`, and `ERL_DRV_EXT2TERM` term types were introduced in ERTS 5.6.

This function is thread-safe.

[](){: id=erl_drv_putenv }
## erl_drv_putenv/2

```c
int erl_drv_putenv(const char *key, char
        *value);
```

[](){: id=erl_drv_putenv }
Sets the value of an environment variable.

`key` is a `NULL`\-terminated string containing the name of the environment variable.

`value` is a `NULL`\-terminated string containing the new value of the environment variable.

Returns `0` on success, otherwise a value `!= 0`.

> #### Note {: class=info }
> The result of passing the empty string (`""`) as a value is platform-dependent. On some platforms the variable value is set to the empty string, on others the environment variable is removed.

> #### Warning {: class=warning }
> This function modifies the emulated environment used by `os:putenv/2` and not the environment used by libc's `m:putenv` or similar. Drivers that *require* that these are in sync will need to do so themselves, but keep in mind that they are segregated for a reason; `m:putenv` and its friends are *not thread-safe* and may cause unrelated code to misbehave or crash the emulator.

This function is thread-safe.

[](){: id=erl_drv_rwlock_create }
## erl_drv_rwlock_create/1

```c
ErlDrvRWLock * erl_drv_rwlock_create(char
        *name);
```

[](){: id=erl_drv_rwlock_create }
Creates an rwlock and returns a pointer to it.

`name` is a string identifying the created rwlock. It is used to identify the rwlock in debug functionality (see note about the [lock checker](erl_driver.md#lock_checker)).

Returns `NULL` on failure. The driver creating the rwlock is responsible for destroying it before the driver is unloaded.

This function is thread-safe.

[](){: id=erl_drv_rwlock_destroy }
## erl_drv_rwlock_destroy/1

```c
void erl_drv_rwlock_destroy(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_destroy }
Destroys an rwlock previously created by [`erl_drv_rwlock_create`](erl_driver.md#erl_drv_rwlock_create). The rwlock must be in an unlocked state before it is destroyed.

`rwlck` is a pointer to an rwlock to destroy.

This function is thread-safe.

[](){: id=erl_drv_rwlock_name }
## erl_drv_rwlock_name/1

```c
char * erl_drv_rwlock_name(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_name }
Returns a pointer to the name of the rwlock.

`rwlck` is a pointer to an initialized rwlock.

> #### Note {: class=info }
> This function is intended for debugging purposes only.

[](){: id=erl_drv_rwlock_rlock }
## erl_drv_rwlock_rlock/1

```c
void erl_drv_rwlock_rlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_rlock }
Read locks an rwlock. The calling thread is blocked until the rwlock has been read locked. A thread that currently has read or read/write locked the rwlock *cannot* lock the same rwlock again.

`rwlck` is a pointer to the rwlock to read lock.

> #### Warning {: class=warning }
> If you leave an rwlock locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_rwlock_runlock }
## erl_drv_rwlock_runlock/1

```c
void erl_drv_rwlock_runlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_runlock }
Read unlocks an rwlock. The rwlock currently must be read locked by the calling thread.

`rwlck` is a pointer to an rwlock to read unlock.

This function is thread-safe.

[](){: id=erl_drv_rwlock_rwlock }
## erl_drv_rwlock_rwlock/1

```c
void erl_drv_rwlock_rwlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_rwlock }
Read/write locks an rwlock. The calling thread is blocked until the rwlock has been read/write locked. A thread that currently has read or read/write locked the rwlock *cannot* lock the same rwlock again.

`rwlck` is a pointer to an rwlock to read/write lock.

> #### Warning {: class=warning }
> If you leave an rwlock locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_rwlock_rwunlock }
## erl_drv_rwlock_rwunlock/1

```c
void erl_drv_rwlock_rwunlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_rwunlock }
Read/write unlocks an rwlock. The rwlock currently must be read/write locked by the calling thread.

`rwlck` is a pointer to an rwlock to read/write unlock.

This function is thread-safe.

[](){: id=erl_drv_rwlock_tryrlock }
## erl_drv_rwlock_tryrlock/1

```c
int erl_drv_rwlock_tryrlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_tryrlock }
Tries to read lock an rwlock.

`rwlck` is a pointer to an rwlock to try to read lock.

Returns `0` on success, otherwise `EBUSY`. A thread that currently has read or read/write locked the rwlock *cannot* try to lock the same rwlock again.

> #### Warning {: class=warning }
> If you leave an rwlock locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_rwlock_tryrwlock }
## erl_drv_rwlock_tryrwlock/1

```c
int erl_drv_rwlock_tryrwlock(ErlDrvRWLock
        *rwlck);
```

[](){: id=erl_drv_rwlock_tryrwlock }
Tries to read/write lock an rwlock. A thread that currently has read or read/write locked the rwlock *cannot* try to lock the same rwlock again.

`rwlck`is pointer to an rwlock to try to read/write lock.

Returns `0` on success, otherwise `EBUSY`.

> #### Warning {: class=warning }
> If you leave an rwlock locked in an emulator thread when you let the thread out of your control, you will *very likely* deadlock the whole emulator.

This function is thread-safe.

[](){: id=erl_drv_send_term }
## erl_drv_send_term/4

```c
int erl_drv_send_term(ErlDrvTermData port,
        ErlDrvTermData receiver, ErlDrvTermData* term, int n);
```

[](){: id=erl_drv_send_term }
This function is the only way for a driver to send data to *other* processes than the port owner process. Parameter `receiver` specifies the process to receive the data.

> #### Note {: class=info }
> Parameter `port` is *not* an ordinary port handle, but a port handle converted using [`driver_mk_port`](erl_driver.md#driver_mk_port).

Parameters `port`, `term`, and `n` work as in [`erl_drv_output_term`](erl_driver.md#erl_drv_output_term).

This function is thread-safe.

[](){: id=erl_drv_set_os_pid }
## erl_drv_set_os_pid/2

```c
void erl_drv_set_os_pid(ErlDrvPort port,
        ErlDrvSInt pid);
```

[](){: id=erl_drv_set_os_pid }
Sets the `os_pid` seen when doing `erlang:port_info/2` on this port.

`port` is the port handle of the port (driver instance) to set the pid on. `pid`is the pid to set.

[](){: id=erl_drv_thread_create }
## erl_drv_thread_create/5

```c
int erl_drv_thread_create(char *name, ErlDrvTid
        *tid, void * (*func)(void *), void *arg, ErlDrvThreadOpts
        *opts);
```

[](){: id=erl_drv_thread_create }
Creates a new thread.

* __`name`__ - A string identifying the created thread. It is used to identify the thread in planned future debug functionality.

* __`tid`__ - A pointer to a thread identifier variable.

* __`func`__ - A pointer to a function to execute in the created thread.

* __`arg`__ - A pointer to argument to the `func` function.

* __`opts`__ - A pointer to thread options to use or `NULL`.

Returns `0` on success, otherwise an `errno` value is returned to indicate the error. The newly created thread begins executing in the function pointed to by `func`, and `func` is passed `arg` as argument. When `erl_drv_thread_create` returns, the thread identifier of the newly created thread is available in `*tid`. `opts` can be either a `NULL` pointer, or a pointer to an [`ErlDrvThreadOpts`](erl_driver.md#erldrvthreadopts) structure. If `opts` is a `NULL` pointer, default options are used, otherwise the passed options are used.

> #### Warning {: class=warning }
> You are not allowed to allocate the [`ErlDrvThreadOpts`](erl_driver.md#erldrvthreadopts) structure by yourself. It must be allocated and initialized by [`erl_drv_thread_opts_create`](erl_driver.md#erl_drv_thread_opts_create).

The created thread terminates either when `func` returns or if [`erl_drv_thread_exit`](erl_driver.md#erl_drv_thread_exit) is called by the thread. The exit value of the thread is either returned from `func` or passed as argument to [`erl_drv_thread_exit`](erl_driver.md#erl_drv_thread_exit). The driver creating the thread is responsible for joining the thread, through [`erl_drv_thread_join`](erl_driver.md#erl_drv_thread_join), before the driver is unloaded. "Detached" threads cannot be created, that is, threads that do not need to be joined.

> #### Warning {: class=warning }
> All created threads must be joined by the driver before it is unloaded. If the driver fails to join all threads created before it is unloaded, the runtime system most likely crashes when the driver code is unloaded.

This function is thread-safe.

[](){: id=erl_drv_thread_exit }
## erl_drv_thread_exit/1

```c
void erl_drv_thread_exit(void
        *exit_value);
```

[](){: id=erl_drv_thread_exit }
Terminates the calling thread with the exit value passed as argument. `exit_value` is a pointer to an exit value or `NULL`.

You are only allowed to terminate threads created with [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create).

The exit value can later be retrieved by another thread through [`erl_drv_thread_join`](erl_driver.md#erl_drv_thread_join).

This function is thread-safe.

[](){: id=erl_drv_thread_join }
## erl_drv_thread_join/2

```c
int erl_drv_thread_join(ErlDrvTid tid, void
        **exit_value);
```

[](){: id=erl_drv_thread_join }
Joins the calling thread with another thread, that is, the calling thread is blocked until the thread identified by `tid` has terminated.

`tid` is the thread identifier of the thread to join. `exit_value` is a pointer to a pointer to an exit value, or `NULL`.

Returns `0` on success, otherwise an `errno` value is returned to indicate the error.

A thread can only be joined once. The behavior of joining more than once is undefined, an emulator crash is likely. If `exit_value == NULL`, the exit value of the terminated thread is ignored, otherwise the exit value of the terminated thread is stored at `*exit_value`.

This function is thread-safe.

[](){: id=erl_drv_thread_name }
## erl_drv_thread_name/1

```c
char * erl_drv_thread_name(ErlDrvTid
        tid);
```

[](){: id=erl_drv_rwlock_name }
Returns a pointer to the name of the thread.

`tid` is a thread identifier.

> #### Note {: class=info }
> This function is intended for debugging purposes only.

[](){: id=erl_drv_thread_opts_create }
## erl_drv_thread_opts_create/1

```c
ErlDrvThreadOpts * erl_drv_thread_opts_create(char *name);
```

[](){: id=erl_drv_thread_opts_create }
Allocates and initializes a thread option structure.

`name` is a string identifying the created thread options. It is used to identify the thread options in planned future debug functionality.

Returns `NULL` on failure. A thread option structure is used for passing options to [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create). If the structure is not modified before it is passed to [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create), the default values are used.

> #### Warning {: class=warning }
> You are not allowed to allocate the [`ErlDrvThreadOpts`](erl_driver.md#erldrvthreadopts) structure by yourself. It must be allocated and initialized by `erl_drv_thread_opts_create`.

This function is thread-safe.

[](){: id=erl_drv_thread_opts_destroy }
## erl_drv_thread_opts_destroy/1

```c
void erl_drv_thread_opts_destroy(ErlDrvThreadOpts *opts);
```

[](){: id=erl_drv_thread_opts_destroy }
Destroys thread options previously created by [`erl_drv_thread_opts_create`](erl_driver.md#erl_drv_thread_opts_create).

`opts` is a pointer to thread options to destroy.

This function is thread-safe.

[](){: id=erl_drv_thread_self }
## erl_drv_thread_self/1

```c
ErlDrvTid erl_drv_thread_self(void);
```

[](){: id=erl_drv_thread_self }
Returns the thread identifier of the calling thread.

This function is thread-safe.

[](){: id=erl_drv_time_offset }
## erl_drv_time_offset/1

```c
ErlDrvTime erl_drv_time_offset(ErlDrvTimeUnit
        time_unit);
```

[](){: id=erl_drv_time_offset }
Returns the current time offset between [Erlang monotonic time](time_correction.md#erlang_monotonic_time) and [Erlang system time](time_correction.md#erlang_system_time) converted into the `time_unit` passed as argument.

`time_unit` is time unit of returned value.

Returns `ERL_DRV_TIME_ERROR` if called with an invalid time unit argument, or if called from a thread that is not a scheduler thread.

See also [`ErlDrvTime`](erl_driver.md#erldrvtime) and [`ErlDrvTimeUnit`](erl_driver.md#erldrvtimeunit).

[](){: id=erl_drv_tsd_get }
## erl_drv_tsd_get/1

```c
void * erl_drv_tsd_get(ErlDrvTSDKey
        key);
```

[](){: id=erl_drv_tsd_get }
Returns the thread-specific data associated with `key` for the calling thread.

`key` is a thread-specific data key.

Returns `NULL` if no data has been associated with `key` for the calling thread.

This function is thread-safe.

[](){: id=erl_drv_tsd_key_create }
## erl_drv_tsd_key_create/2

```c
int erl_drv_tsd_key_create(char *name,
        ErlDrvTSDKey *key);
```

[](){: id=erl_drv_tsd_key_create }
Creates a thread-specific data key.

`name` is a string identifying the created key. It is used to identify the key in planned future debug functionality.

`key` is a pointer to a thread-specific data key variable.

Returns `0` on success, otherwise an `errno` value is returned to indicate the error. The driver creating the key is responsible for destroying it before the driver is unloaded.

This function is thread-safe.

[](){: id=erl_drv_tsd_key_destroy }
## erl_drv_tsd_key_destroy/1

```c
void erl_drv_tsd_key_destroy(ErlDrvTSDKey
        key);
```

[](){: id=erl_drv_tsd_key_destroy }
Destroys a thread-specific data key previously created by [`erl_drv_tsd_key_create`](erl_driver.md#erl_drv_tsd_key_create). All thread-specific data using this key in all threads must be cleared (see [`erl_drv_tsd_set`](erl_driver.md#erl_drv_tsd_set)) before the call to `erl_drv_tsd_key_destroy`.

`key` is a thread-specific data key to destroy.

> #### Warning {: class=warning }
> A destroyed key is very likely to be reused soon. Therefore, if you fail to clear the thread-specific data using this key in a thread before destroying the key, you will *very likely* get unexpected errors in other parts of the system.

This function is thread-safe.

[](){: id=erl_drv_tsd_set }
## erl_drv_tsd_set/2

```c
void erl_drv_tsd_set(ErlDrvTSDKey key, void
        *data);
```

[](){: id=erl_drv_tsd_set }
Sets thread-specific data associated with `key` for the calling thread. You are only allowed to set thread-specific data for threads while they are fully under your control. For example, if you set thread-specific data in a thread calling a driver callback function, it must be cleared, that is, set to `NULL`, before returning from the driver callback function.

`key` is a thread-specific data key.

`data` is a pointer to data to associate with `key` in the calling thread.

> #### Warning {: class=warning }
> If you fail to clear thread-specific data in an emulator thread before letting it out of your control, you might never be able to clear this data with later unexpected errors in other parts of the system as a result.

This function is thread-safe.

[](){: id=erl_errno_id }
## erl_errno_id/1

```c
char * erl_errno_id(int error);
```

[](){: id=erl_errno_id }
Returns the atom name of the Erlang error, given the error number in `error`. The error atoms are `einval`, `enoent`, and so on. It can be used to make error terms from the driver.

[](){: id=remove_driver_entry }
## remove_driver_entry/1

```c
int remove_driver_entry(ErlDrvEntry
        *de);
```

[](){: id=remove_driver_entry }
Removes a driver entry `de` previously added with [`add_driver_entry`](erl_driver.md#add_driver_entry).

Driver entries added by the `erl_ddll` Erlang interface cannot be removed by using this interface.

[](){: id=set_busy_port }
## set_busy_port/2

```c
void set_busy_port(ErlDrvPort port, int
        on);
```

[](){: id=set_busy_port }
Sets and unsets the busy state of the port. If `on` is non-zero, the port is set to busy. If it is zero, the port is set to not busy. You typically want to combine this feature with the [busy port message queue](erl_driver.md#erl_drv_busy_msgq_limits) functionality.

Processes sending command data to the port are suspended if either the port or the port message queue is busy. Suspended processes are resumed when neither the port or the port message queue is busy. Command data is in this context data passed to the port using either `Port ! {Owner, {command, Data}}` or `port_command/[2,3]`.

If the [ERL_DRV_FLAG_SOFT_BUSY](driver_entry.md#driver_flags) has been set in the [`driver_entry`](driver_entry.md), data can be forced into the driver through [`erlang:port_command(Port, Data, [force])`](`erlang:port_command/3`) even if the driver has signaled that it is busy.

For information about busy port message queue functionality, see [`erl_drv_busy_msgq_limits`](erl_driver.md#erl_drv_busy_msgq_limits).

[](){: id=set_port_control_flags }
## set_port_control_flags/2

```c
void set_port_control_flags(ErlDrvPort port,
        int flags);
```

[](){: id=set_port_control_flags }
Sets flags for how the [`control`](driver_entry.md#control) driver entry function will return data to the port owner process. (The `control` function is called from `erlang:port_control/3`.)

Currently there are only two meaningful values for `flags`: `0` means that data is returned in a list, and `PORT_CONTROL_FLAG_BINARY` means data is returned as a binary from `control`.

## See Also

[`driver_entry(3)`](driver_entry.md), `m:erlang`, `m:erl_ddll`, section [How to Implement an Alternative Carrier for the Erlang Distribution](alt_dist.md) in the User's Guide
