Beam SSA IR
===========

A SSA IR is used by the beam loader to make load-time optimization. The most
prominent optimization that can be made is the inlining of cross module
function calls. The BEAM compiler may include a chunk in the .beam file
with the SSA, or if the compiler is present in run-time the code_server
can transform the normal beam assembly to SSA IR.

Both the BEAM assembly and the SSA IR is included in the .beam file in order
to speed up loading of files. The SSA IR is only used after the system has
run for a while without loading any more files. This because we do not
want short shell programs to spend lots of time doing optimizations.

Format
------

The IR has a format that is very close to what beam assembly looks like.
The main differences are that all `move` and `allocate` instructions
have been removed and instead a SSA scheme is used.

    <<Opcode:8, Users:16, Uses:16/binary>>
    <<Opcode:8, Users:16, NumUses:8, Uses:24/binary>>

The number of uses of each instruction will vary, but the majority is < 4.
Some of the instructions have dynamic arity, i.e. `select_val`, then the
second format it used.

The `Users:16` field contains two values; first a 8-bit relative index
to the instruction that Uses this instruction, and a 8-bit index
to say which `Use` of that instruction is the `User`.

If an instruction has multiple `Users`, they are chained in the `Use`
part of the `User`. e.g.

    0: a0 := getarg 0
    1: a1 := getarg 1
    2: v0 := get_tuple_element a0 a1
    3: v1 := add v0 v0
    4: return v1

would be encoded as

    getarg:8 2:8 0:8 0:8
    getarg:8 2:8 0:8 1:8
    get_element_tuple:8 3:8 0:8 0:8 0:16 1:8 0:16
