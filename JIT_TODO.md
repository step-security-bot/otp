# asmbeam

## Known bugs
* memory leaks in code re-loading
* `erts_schedule_bif` is broken because it checks *I
  * Because of this, trapping has been disabled for `integer_to_binary/1,2` and
    `integer_to_list/1,2`

## Missing fetures

* tracing
* line numbers
* save_calls
  * Make this optional at start-time with flag
  * Store `neg_o_reds` on stack

## Refactor

Goal is to make it possible for the beam interpreter and the beam assembler
to live in the same repo with too much interference with eachother. This means
that we want to limit the amount of files where we need to take both into'
consideration.

### Suggestion

Introduce these new files:
  * beam\_load\_asm.cpp
    * Possibly this will be a cpp file... contains loading logic for asm
  * beam\_load\_emu.c
    * This is basically a rename of today beam\_load.c, only that only the
      beam_emu specific parts are in there.
  * beam\_debug\_emu.c
    * Debugging for emulator
  * beam\_debug\_asm.cpp
    * Debugging for asm
  * beam_asm.cpp
    * The place where process_main lives for asm

Repurpose beam\_load.c to be used for the common part of loading code.

Fix Beam opcode macros erl_vm.h to work for both emu and asm.

## Future ideas

* Move fregs, regs and EBS into single struct and use offsets into it
  * Will free up a register that can be used for x0?
* Build match specs using asmjit
* Hoist `struct enif_environment_t` to stack of thread and only do what is needed by `erts_pre_nif`.
* Move more code into Global asm space to reduce code size. Example, the implementation of `emit_cmp_spec` could be made global with a custom CC.
* Add a naive register allocator to the loader
  * Helps with correctness of emit_* code and if we move x0+x1 to registers we can
    use it to allocate things better there
* Use native stack as erlang stack?
  * What to do about signals?
