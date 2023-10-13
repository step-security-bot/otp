# erts_alloc

An Erlang runtime system internal memory allocator library.

## Description

`erts_alloc` is an Erlang runtime system internal memory allocator library. `erts_alloc` provides the Erlang runtime system with a number of memory allocators.

## Allocators

[](){: id=allocators }
The following allocators are present:

* __`temp_alloc`__ - Allocator used for temporary allocations.

* __`eheap_alloc`__ - Allocator used for Erlang heap data, such as Erlang process heaps.

* __`binary_alloc`__ - Allocator used for Erlang binary data.

* __`ets_alloc`__ - Allocator used for `ets` data.

* __`driver_alloc`__ - Allocator used for driver data.

* __`literal_alloc`__ - Allocator used for constant terms in Erlang code.

* __`sl_alloc`__ - Allocator used for memory blocks that are expected to be short-lived.

* __`ll_alloc`__ - Allocator used for memory blocks that are expected to be long-lived, for example, Erlang code.

* __`fix_alloc`__ - A fast allocator used for some frequently used fixed size data types.

* __`std_alloc`__ - Allocator used for most memory blocks not allocated through any of the other allocators described above.

* __`sys_alloc`__ - This is normally the default `malloc` implementation used on the specific OS.

* __`mseg_alloc`__ - A memory segment allocator. It is used by other allocators for allocating memory segments and is only available on systems that have the `mmap` system call. Memory segments that are deallocated are kept for a while in a segment cache before they are destroyed. When segments are allocated, cached segments are used if possible instead of creating new segments. This to reduce the number of system calls made.

`sys_alloc`, `literal_alloc` and `temp_alloc` are always enabled and cannot be disabled. `mseg_alloc` is always enabled if it is available and an allocator that uses it is enabled. All other allocators can be [enabled or disabled](erts_alloc.md#m_e). By default all allocators are enabled. When an allocator is disabled, `sys_alloc` is used instead of the disabled allocator.

The main idea with the `erts_alloc` library is to separate memory blocks that are used differently into different memory areas, to achieve less memory fragmentation. By putting less effort in finding a good fit for memory blocks that are frequently allocated than for those less frequently allocated, a performance gain can be achieved.

[](){: id=alloc_util }
## The alloc_util Framework

Internally a framework called `alloc_util` is used for implementing allocators. `sys_alloc` and `mseg_alloc` do not use this framework, so the following does *not* apply to them.

An allocator manages multiple areas, called carriers, in which memory blocks are placed. A carrier is either placed in a separate memory segment (allocated through `mseg_alloc`), or in the heap segment (allocated through `sys_alloc`).

* Multiblock carriers are used for storage of several blocks.
* Singleblock carriers are used for storage of one block.
* Blocks that are larger than the value of the singleblock carrier threshold ([`sbct`](erts_alloc.md#m_sbct)) parameter are placed in singleblock carriers.
* Blocks that are smaller than the value of parameter `sbct` are placed in multiblock carriers.

Normally an allocator creates a "main multiblock carrier". Main multiblock carriers are never deallocated. The size of the main multiblock carrier is determined by the value of parameter [`mmbcs`](erts_alloc.md#m_mmbcs).

[](){: id=mseg_mbc_sizes }
Sizes of multiblock carriers allocated through `mseg_alloc` are decided based on the following parameters:

* The values of the largest multiblock carrier size ([`lmbcs`](erts_alloc.md#m_lmbcs))
* The smallest multiblock carrier size ([`smbcs`](erts_alloc.md#m_smbcs))
* The multiblock carrier growth stages ([`mbcgs`](erts_alloc.md#m_mbcgs))

If `nc` is the current number of multiblock carriers (the main multiblock carrier excluded) managed by an allocator, the size of the next `mseg_alloc` multiblock carrier allocated by this allocator is roughly `smbcs+nc*(lmbcs-smbcs)/mbcgs` when `nc <= mbcgs`, and `lmbcs` when `nc > mbcgs`. If the value of parameter `sbct` is larger than the value of parameter `lmbcs`, the allocator may have to create multiblock carriers that are larger than the value of parameter `lmbcs`, though. Singleblock carriers allocated through `mseg_alloc` are sized to whole pages.

Sizes of carriers allocated through `sys_alloc` are decided based on the value of the `sys_alloc` carrier size ([`ycs`](erts_alloc.md#muycs)) parameter. The size of a carrier is the least number of multiples of the value of parameter `ycs` satisfying the request.

Coalescing of free blocks are always performed immediately. Boundary tags (headers and footers) in free blocks are used, which makes the time complexity for coalescing constant.

[](){: id=strategy }
The memory allocation strategy used for multiblock carriers by an allocator can be configured using parameter [`as`](erts_alloc.md#m_as). The following strategies are available:

* __Best fit__ - Strategy: Find the smallest block satisfying the requested block size.

  Implementation: A balanced binary search tree is used. The time complexity is proportional to log N, where N is the number of sizes of free blocks.

* __Address order best fit__ - Strategy: Find the smallest block satisfying the requested block size. If multiple blocks are found, choose the one with the lowest address.

  Implementation: A balanced binary search tree is used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Address order first fit__ - Strategy: Find the block with the lowest address satisfying the requested block size.

  Implementation: A balanced binary search tree is used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Address order first fit carrier best fit__ - Strategy: Find the *carrier* with the lowest address that can satisfy the requested block size, then find a block within that carrier using the "best fit" strategy.

  Implementation: Balanced binary search trees are used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Address order first fit carrier address order best fit__ - Strategy: Find the *carrier* with the lowest address that can satisfy the requested block size, then find a block within that carrier using the "address order best fit" strategy.

  Implementation: Balanced binary search trees are used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Age order first fit carrier address order first fit__ - Strategy: Find the *oldest carrier* that can satisfy the requested block size, then find a block within that carrier using the "address order first fit" strategy.

  Implementation: A balanced binary search tree is used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Age order first fit carrier best fit__ - Strategy: Find the *oldest carrier* that can satisfy the requested block size, then find a block within that carrier using the "best fit" strategy.

  Implementation: Balanced binary search trees are used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Age order first fit carrier address order best fit__ - Strategy: Find the *oldest carrier* that can satisfy the requested block size, then find a block within that carrier using the "address order best fit" strategy.

  Implementation: Balanced binary search trees are used. The time complexity is proportional to log N, where N is the number of free blocks.

* __Good fit__ - Strategy: Try to find the best fit, but settle for the best fit found during a limited search.

  Implementation: The implementation uses segregated free lists with a maximum block search depth (in each list) to find a good fit fast. When the maximum block search depth is small (by default 3), this implementation has a time complexity that is constant. The maximum block search depth can be configured using parameter [`mbsd`](erts_alloc.md#m_mbsd).

* __A fit__ - Strategy: Do not search for a fit, inspect only one free block to see if it satisfies the request. This strategy is only intended to be used for temporary allocations.

  Implementation: Inspect the first block in a free-list. If it satisfies the request, it is used, otherwise a new carrier is created. The implementation has a time complexity that is constant.

  As from ERTS 5.6.1 the emulator refuses to use this strategy on other allocators than `temp_alloc`. This because it only causes problems for other allocators.

Apart from the ordinary allocators described above, some pre-allocators are used for some specific data types. These pre-allocators pre-allocate a fixed amount of memory for certain data types when the runtime system starts. As long as pre-allocated memory is available, it is used. When no pre-allocated memory is available, memory is allocated in ordinary allocators. These pre-allocators are typically much faster than the ordinary allocators, but can only satisfy a limited number of requests.

[](){: id=flags }
## System Flags Effecting erts_alloc

> #### Warning {: class=warning }
> Only use these flags if you are sure what you are doing. Unsuitable settings can cause serious performance degradation and even a system crash at any time during operation.

Memory allocator system flags have the following syntax: `+M<S><P> <V>`, where `<S>` is a letter identifying a subsystem, `<P>` is a parameter, and `<V>` is the value to use. The flags can be passed to the Erlang emulator ([`erl(1)`](erl_cmd.md)) as command-line arguments.

System flags effecting specific allocators have an uppercase letter as `<S>`. The following letters are used for the allocators:

* `B: binary_alloc`
* `D: std_alloc`
* `E: ets_alloc`
* `F: fix_alloc`
* `H: eheap_alloc`
* `I: literal_alloc`
* `L: ll_alloc`
* `M: mseg_alloc`
* `R: driver_alloc`
* `S: sl_alloc`
* `T: temp_alloc`
* `Y: sys_alloc`

### Flags for Configuration of mseg_alloc

* __`+MMamcbf <size>`{: id=MMamcbf }__ - Absolute maximum cache bad fit (in kilobytes). A segment in the memory segment cache is not reused if its size exceeds the requested size with more than the value of this parameter. Defaults to `4096`.

* __`+MMrmcbf <ratio>`{: id=MMrmcbf }__ - Relative maximum cache bad fit (in percent). A segment in the memory segment cache is not reused if its size exceeds the requested size with more than relative maximum cache bad fit percent of the requested size. Defaults to `20`.

* __`+MMsco true|false`{: id=MMsco }__ - Sets [super carrier](erts_alloc.md#mmscs) only flag. Defaults to `true`. When a super carrier is used and this flag is `true`, `mseg_alloc` only creates carriers in the super carrier. Notice that the `alloc_util` framework can create `sys_alloc` carriers, so if you want all carriers to be created in the super carrier, you therefore want to disable use of `sys_alloc` carriers by also passing [`+Musac false`](erts_alloc.md#musac). When the flag is `false`, `mseg_alloc` tries to create carriers outside of the super carrier when the super carrier is full.

  > #### Note {: class=info }
  > Setting this flag to `false` is not supported on all systems. The flag is then ignored.

* __`+MMscrfsd <amount>`{: id=MMscrfsd }__ - Sets [super carrier](erts_alloc.md#mmscs) reserved free segment descriptors. Defaults to `65536`. This parameter determines the amount of memory to reserve for free segment descriptors used by the super carrier. If the system runs out of reserved memory for free segment descriptors, other memory is used. This can however cause fragmentation issues, so you want to ensure that this never happens. The maximum amount of free segment descriptors used can be retrieved from the `erts_mmap` tuple part of the result from calling [`erlang:system_info({allocator, mseg_alloc})`](`m:erlang#system_info_allocator_tuple`).

* __`+MMscrpm true|false`{: id=MMscrpm }__ - Sets [super carrier](erts_alloc.md#mmscs) reserve physical memory flag. Defaults to `true`. When this flag is `true`, physical memory is reserved for the whole super carrier at once when it is created. The reservation is after that left unchanged. When this flag is set to `false`, only virtual address space is reserved for the super carrier upon creation. The system attempts to reserve physical memory upon carrier creations in the super carrier, and attempt to unreserve physical memory upon carrier destructions in the super carrier.

  > #### Note {: class=info }
  > What reservation of physical memory means, highly depends on the operating system, and how it is configured. For example, different memory overcommit settings on Linux drastically change the behavior.
  >
  > Setting this flag to `false` is possibly not supported on all systems. The flag is then ignored.

* __`+MMscs <size in MB>`{: id=MMscs }__ - Sets super carrier size (in MB). Defaults to `0`, that is, the super carrier is by default disabled. The super carrier is a large continuous area in the virtual address space. `mseg_alloc` always tries to create new carriers in the super carrier if it exists. Notice that the `alloc_util` framework can create `sys_alloc` carriers. For more information, see [`+MMsco`](erts_alloc.md#mmsco).

* __`+MMmcs <amount>`{: id=MMmcs }__ - Maximum cached segments. The maximum number of memory segments stored in the memory segment cache. Valid range is `[0, 30]`. Defaults to `10`.

### Flags for Configuration of sys_alloc

* __`+MYe true`{: id=MYe }__ - Enables `sys_alloc`.

  > #### Note {: class=info }
  > `sys_alloc` cannot be disabled.

* __`+MYtt <size>`{: id=MYtt }__ - Trim threshold size (in kilobytes). This is the maximum amount of free memory at the top of the heap (allocated by `sbrk`) that is kept by `malloc` (not released to the operating system). When the amount of free memory at the top of the heap exceeds the trim threshold, `malloc` releases it (by calling `sbrk`). Trim threshold is specified in kilobytes. Defaults to `128`.

  > #### Note {: class=info }
  > This flag has effect only when the emulator is linked with the GNU C library, and uses its `malloc` implementation.

* __`+MYtp <size>`{: id=MYtp }__ - Top pad size (in kilobytes). This is the amount of extra memory that is allocated by `malloc` when `sbrk` is called to get more memory from the operating system. Defaults to `0`.

  > #### Note {: class=info }
  > This flag has effect only when the emulator is linked with the GNU C library, and uses its `malloc` implementation.

### Flags for Configuration of Allocators Based on alloc_util

If `u` is used as subsystem identifier (that is, `<S> = u`), all allocators based on `alloc_util` are effected. If `B`, `D`, `E`, `F`, `H`, `I`, `L`, `R`, `S`, `T`, `X` is used as subsystem identifier, only the specific allocator identifier is effected.

* __`+M<S>acul <utilization>|de`{: id=M_acul }__ - Abandon carrier utilization limit. A valid `<utilization>` is an integer in the range `[0, 100]` representing utilization in percent. When a utilization value > 0 is used, allocator instances are allowed to abandon multiblock carriers. If `de` (default enabled) is passed instead of a `<utilization>`, a recommended non-zero utilization value is used. The value chosen depends on the allocator type and can be changed between ERTS versions. Defaults to `de`, but this can be changed in the future.

  Carriers are abandoned when memory utilization in the allocator instance falls below the utilization value used. Once a carrier is abandoned, no new allocations are made in it. When an allocator instance gets an increased multiblock carrier need, it first tries to fetch an abandoned carrier from another allocator instance. If no abandoned carrier can be fetched, it creates a new empty carrier. When an abandoned carrier has been fetched, it will function as an ordinary carrier. This feature has special requirements on the [allocation strategy](erts_alloc.md#m_as) used. Only the strategies `aoff`, `aoffcbf`, `aoffcaobf`, `ageffcaoff`m, `ageffcbf` and `ageffcaobf` support abandoned carriers.

  This feature also requires [multiple thread specific instances](erts_alloc.md#m_t) to be enabled. When enabling this feature, multiple thread-specific instances are enabled if not already enabled, and the `aoffcbf` strategy is enabled if the current strategy does not support abandoned carriers. This feature can be enabled on all allocators based on the `alloc_util` framework, except `temp_alloc` (which would be pointless).

* __`+M<S>acfml <bytes>`{: id=M_acfml }__ - Abandon carrier free block min limit. A valid `<bytes>` is a positive integer representing a block size limit. The largest free block in a carrier must be at least `bytes` large, for the carrier to be abandoned. The default is zero but can be changed in the future.

  See also [`acul`](erts_alloc.md#m_acul).

* __`+M<S>acnl <amount>`{: id=M_acnl }__ - Abandon carrier number limit. A valid `<amount>` is a positive integer representing max number of abandoned carriers per allocator instance. Defaults to 1000 which will practically disable the limit, but this can be changed in the future.

  See also [`acul`](erts_alloc.md#m_acul).

* __`+M<S>acful <utilization>|de`{: id=M_acful }__ - Abandon carrier free utilization limit. When the utilization of a carrier falls belows this limit erts_alloc instructs the OS that unused memory in the carrier can be re-used for allocation by other OS procesesses. On Unix this is done by calling `madvise(..., ..., MADV_FREE)` on the unused memory region, on Windows it is done by calling `VirtualAlloc(..., ..., MEM_RESET, PAGE_READWRITE)`. Defaults to 0 which means that no memory will be marked as re-usable by the OS.

  A valid `<utilization>` is an integer in the range `[0, 100]` representing utilization in percent. If this value is larger than the `acul` limit it will be lowered to the current `acul` limit. If `de` (default enabled) is passed instead of a `<utilization>`, a recommended non-zero utilization value is used. The value chosen depends on the allocator type and can be changed between ERTS versions.

  See also [`acul`](erts_alloc.md#m_acul).

* __`+M<S>as bf|aobf|aoff|aoffcbf|aoffcaobf|ageffcaoff|ageffcbf|ageffcaobf|gf|af`{: id=M_as }__ - Allocation strategy. The following strategies are valid:

  * `bf` (best fit)
  * `aobf` (address order best fit)
  * `aoff` (address order first fit)
  * `aoffcbf` (address order first fit carrier best fit)
  * `aoffcaobf` (address order first fit carrier address order best fit)
  * `ageffcaoff` (age order first fit carrier address order first fit)
  * `ageffcbf` (age order first fit carrier best fit)
  * `ageffcaobf` (age order first fit carrier address order best fit)
  * `gf` (good fit)
  * `af` (a fit)

  See the description of allocation strategies in section [The alloc_util Framework](erts_alloc.md#strategy).

* __`+M<S>asbcst <size>`{: id=M_asbcst }__ - Absolute singleblock carrier shrink threshold (in kilobytes). When a block located in an `mseg_alloc` singleblock carrier is shrunk, the carrier is left unchanged if the amount of unused memory is less than this threshold, otherwise the carrier is shrunk. See also [`rsbcst`](erts_alloc.md#m_rsbcst).

* __`+M<S>atags true|false`{: id=M_atags }__ - Adds a small tag to each allocated block that contains basic information about what it is and who allocated it. Use the `m:instrument` module to inspect this information.

  The runtime overhead is two words per allocation when enabled. This may change at any time in the future.

  The default is `true` for `binary_alloc` and `driver_alloc`, and `false` for the other allocator types.

* __`+M<S>cp B|D|E|F|H||L|R|S|@|:`{: id=M_cp }__ - Set carrier pool to use for the allocator. Memory carriers will only migrate between allocator instances that use the same carrier pool. The following carrier pool names exist:

  * __`B`__ - Carrier pool associated with `binary_alloc`.

  * __`D`__ - Carrier pool associated with `std_alloc`.

  * __`E`__ - Carrier pool associated with `ets_alloc`.

  * __`F`__ - Carrier pool associated with `fix_alloc`.

  * __`H`__ - Carrier pool associated with `eheap_alloc`.

  * __`L`__ - Carrier pool associated with `ll_alloc`.

  * __`R`__ - Carrier pool associated with `driver_alloc`.

  * __`S`__ - Carrier pool associated with `sl_alloc`.

  * __`@`__ - Carrier pool associated with the system as a whole.

  Besides passing carrier pool name as value to the parameter, you can also pass `:`. By passing `:` instead of carrier pool name, the allocator will use the carrier pool associated with itself. By passing the command line argument "`+Mucg :`", all allocators that have an associated carrier pool will use the carrier pool associated with themselves.

  The association between carrier pool and allocator is very loose. The associations are more or less only there to get names for the amount of carrier pools needed and names of carrier pools that can be easily identified by the `:` value.

  This flag is only valid for allocators that have an associated carrier pool. Besides that, there are no restrictions on carrier pools to use for an allocator.

  Currently each allocator with an associated carrier pool defaults to using its own associated carrier pool.

* __`+M<S>e true|false`{: id=M_e }__ - Enables allocator `<S>`.

* __`+M<S>lmbcs <size>`{: id=M_lmbcs }__ - Largest (`mseg_alloc`) multiblock carrier size (in kilobytes). See the description on how sizes for `mseg_alloc` multiblock carriers are decided in section [The alloc_util Framework](erts_alloc.md#mseg_mbc_sizes). On 32-bit Unix style OS this limit cannot be set > 64 MB.

* __`+M<S>mbcgs <ratio>`{: id=M_mbcgs }__ - (`mseg_alloc`) multiblock carrier growth stages. See the description on how sizes for `mseg_alloc` multiblock carriers are decided in section [The alloc_util Framework](erts_alloc.md#mseg_mbc_sizes).

* __`+M<S>mbsd <depth>`{: id=M_mbsd }__ - Maximum block search depth. This flag has effect only if the good fit strategy is selected for allocator `<S>`. When the good fit strategy is used, free blocks are placed in segregated free-lists. Each free-list contains blocks of sizes in a specific range. The maximum block search depth sets a limit on the maximum number of blocks to inspect in a free-list during a search for suitable block satisfying the request.

* __`+M<S>mmbcs <size>`{: id=M_mmbcs }__ - Main multiblock carrier size. Sets the size of the main multiblock carrier for allocator `<S>`. The main multiblock carrier is allocated through `sys_alloc` and is never deallocated.

* __`+M<S>mmmbc <amount>`{: id=M_mmmbc }__ - Maximum `mseg_alloc` multiblock carriers. Maximum number of multiblock carriers allocated through `mseg_alloc` by allocator `<S>`. When this limit is reached, new multiblock carriers are allocated through `sys_alloc`.

* __`+M<S>mmsbc <amount>`{: id=M_mmsbc }__ - Maximum `mseg_alloc` singleblock carriers. Maximum number of singleblock carriers allocated through `mseg_alloc` by allocator `<S>`. When this limit is reached, new singleblock carriers are allocated through `sys_alloc`.

* __`+M<S>ramv <bool>`{: id=M_ramv }__ - Realloc always moves. When enabled, reallocate operations are more or less translated into an allocate, copy, free sequence. This often reduces memory fragmentation, but costs performance.

* __`+M<S>rmbcmt <ratio>`{: id=M_rmbcmt }__ - Relative multiblock carrier move threshold (in percent). When a block located in a multiblock carrier is shrunk, the block is moved if the ratio of the size of the freed memory compared to the previous size is more than this threshold, otherwise the block is shrunk at the current location.

* __`+M<S>rsbcmt <ratio>`{: id=M_rsbcmt }__ - Relative singleblock carrier move threshold (in percent). When a block located in a singleblock carrier is shrunk to a size smaller than the value of parameter [`sbct`](erts_alloc.md#m_sbct), the block is left unchanged in the singleblock carrier if the ratio of unused memory is less than this threshold, otherwise it is moved into a multiblock carrier.

* __`+M<S>rsbcst <ratio>`{: id=M_rsbcst }__ - Relative singleblock carrier shrink threshold (in percent). When a block located in an `mseg_alloc` singleblock carrier is shrunk, the carrier is left unchanged if the ratio of unused memory is less than this threshold, otherwise the carrier is shrunk. See also [`asbcst`](erts_alloc.md#m_asbcst).

* __`+M<S>sbct <size>`{: id=M_sbct }__ - Singleblock carrier threshold (in kilobytes). Blocks larger than this threshold are placed in singleblock carriers. Blocks smaller than this threshold are placed in multiblock carriers. On 32-bit Unix style OS this threshold cannot be set > 8 MB.

* __`+M<S>smbcs <size>`{: id=M_smbcs }__ - Smallest (`mseg_alloc`) multiblock carrier size (in kilobytes). See the description on how sizes for `mseg_alloc` multiblock carriers are decided in section [The alloc_util Framework](erts_alloc.md#mseg_mbc_sizes).

* __`+M<S>t true|false`{: id=M_t }__ - Multiple, thread-specific instances of the allocator. Default behavior is `NoSchedulers+1` instances. Each scheduler uses a lock-free instance of its own and other threads use a common instance.

  Before ERTS 5.9 it was possible to configure a smaller number of thread-specific instances than schedulers. This is, however, not possible anymore.

### Flags for Configuration of alloc_util

All allocators based on `alloc_util` are effected.

* __`+Muycs <size>`{: id=Muycs }__ - `sys_alloc` carrier size. Carriers allocated through `sys_alloc` are allocated in sizes that are multiples of the `sys_alloc` carrier size. This is not true for main multiblock carriers and carriers allocated during a memory shortage, though.

* __`+Mummc <amount>`{: id=Mummc }__ - Maximum `mseg_alloc` carriers. Maximum number of carriers placed in separate memory segments. When this limit is reached, new carriers are placed in memory retrieved from `sys_alloc`.

* __`+Musac <bool>`{: id=Musac }__ - Allow `sys_alloc` carriers. Defaults to `true`. If set to `false`, `sys_alloc` carriers are never created by allocators using the `alloc_util` framework.

### Special Flag for literal_alloc

* __`+MIscs <size in MB>`{: id=MIscs }__ - `literal_alloc` super carrier size (in MB). The amount of *virtual* address space reserved for literal terms in Erlang code on 64-bit architectures. Defaults to `1024` (that is, 1 GB), which is usually sufficient. The flag is ignored on 32-bit architectures.

### Instrumentation Flags

* __`+M<S>atags`__ - Adds a small tag to each allocated block that contains basic information about what it is and who allocated it. See [`+M<S>atags`](erts_alloc.md#m_atags) for a more complete description.

> #### Note {: class=info }
> When instrumentation of the emulator is enabled, the emulator uses more memory and runs slower.

### Other Flags

* __`+Mea min|max|r9c|r10b|r11b|config`{: id=Mea }__ - Options:

  * __`min`__ - Disables all allocators that can be disabled.

  * __`max`__ - Enables all allocators (default).

  * __`r9c|r10b|r11b`__ - Configures all allocators as they were configured in respective Erlang/OTP release. These will eventually be removed.

  

* __`+Mlpm all|no`{: id=Mlpm }__ - Lock physical memory. Defaults to `no`, that is, no physical memory is locked. If set to `all`, all memory mappings made by the runtime system are locked into physical memory. If set to `all`, the runtime system fails to start if this feature is not supported, the user has not got enough privileges, or the user is not allowed to lock enough physical memory. The runtime system also fails with an out of memory condition if the user limit on the amount of locked memory is reached.

* __`+Mdai max|<amount>`{: id=Mdai }__ - Set amount of dirty allocator instances used. Defaults to `0`. That is, by default no instances will be used. The maximum amount of instances equals the amount of dirty CPU schedulers on the system.

  By default, each normal scheduler thread has its own allocator instance for each allocator. All other threads in the system, including dirty schedulers, share one instance for each allocator. By enabling dirty allocator instances, dirty schedulers will get and use their own set of allocator instances. Note that these instances are not exclusive to each dirty scheduler, but instead shared among dirty schedulers. The more instances used the less risk of lock contention on these allocator instances. Memory consumption do however increase with increased amount of dirty allocator instances.

## Notes

Only some default values have been presented here. For information about the currently used settings and the current status of the allocators, see [`erlang:system_info(allocator)`](`m:erlang#system_info_allocator`) and [`erlang:system_info({allocator, Alloc})`](`m:erlang#system_info_allocator_tuple`).

> #### Note {: class=info }
> Most of these flags are highly implementation-dependent and can be changed or removed without prior notice.
>
> `erts_alloc` is not obliged to strictly use the settings that have been passed to it (it can even ignore them).

## See Also

[`erl(1)`](erl_cmd.md), `m:erlang`, `m:instrument`
