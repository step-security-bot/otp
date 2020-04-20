# asmbeam

## Known bugs
* memory leaks in code re-loading

## Missing fetures

* on_load
* tracing
* resume bif execution
* line numbers
* save_calls

## Future ideas

* Move fregs, regs and EBS into single struct and use offsets into it
  * Will free up a register that can be used for x0?
* Build match specs using asmjit
* Hoist `struct enif_environment_t` to stack of thread and only do what is needed by `erts_pre_nif`.
* Move more code into Global asm space to reduce code size. Example, the implementation of `emit_cmp_spec` could be made global with a custom CC.
