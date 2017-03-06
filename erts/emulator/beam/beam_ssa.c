/**
 * Convert beam asm to a compact SSA IR, useful for doing small inlining
 * optimizations in load time.
 * Basic form:
 *
 * <<OpCode:8, Uses:8, Args:8, 

 * OpCode: The opcode number, i.e. call, label, move etc.
 *         For Opcodes with < 4 args the arity is encoded into the opcode.
 * Uses: An index to the IR constant area, it contains a chain of indexes
 *       to instructions that use this instruction and where in the instruction
 *       the use is.
 * Args: The number of arguments to this instruction iff Args > 3.
 **/
