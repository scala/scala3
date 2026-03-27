package dotty.tools
package backend
package jvm

/** Test operation. */
enum TestOp:
  case EQ, NE, LT, LE, GT, GE

  /** Returns the negation of this operation. */
  def negate(): TestOp = this match
    case EQ => NE
    case NE => EQ
    case LT => GE
    case GE => LT
    case LE => GT
    case GT => LE

  def opcodeIF(): Int = this match
    case EQ => scala.tools.asm.Opcodes.IFEQ
    case NE => scala.tools.asm.Opcodes.IFNE
    case LT => scala.tools.asm.Opcodes.IFLT
    case GE => scala.tools.asm.Opcodes.IFGE
    case LE => scala.tools.asm.Opcodes.IFLE
    case GT => scala.tools.asm.Opcodes.IFGT

  def opcodeIFICMP(): Int = this match
    case EQ => scala.tools.asm.Opcodes.IF_ICMPEQ
    case NE => scala.tools.asm.Opcodes.IF_ICMPNE
    case LT => scala.tools.asm.Opcodes.IF_ICMPLT
    case GE => scala.tools.asm.Opcodes.IF_ICMPGE
    case LE => scala.tools.asm.Opcodes.IF_ICMPLE
    case GT => scala.tools.asm.Opcodes.IF_ICMPGT