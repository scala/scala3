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
    case EQ => org.objectweb.asm.Opcodes.IFEQ
    case NE => org.objectweb.asm.Opcodes.IFNE
    case LT => org.objectweb.asm.Opcodes.IFLT
    case GE => org.objectweb.asm.Opcodes.IFGE
    case LE => org.objectweb.asm.Opcodes.IFLE
    case GT => org.objectweb.asm.Opcodes.IFGT

  def opcodeIFICMP(): Int = this match
    case EQ => org.objectweb.asm.Opcodes.IF_ICMPEQ
    case NE => org.objectweb.asm.Opcodes.IF_ICMPNE
    case LT => org.objectweb.asm.Opcodes.IF_ICMPLT
    case GE => org.objectweb.asm.Opcodes.IF_ICMPGE
    case LE => org.objectweb.asm.Opcodes.IF_ICMPLE
    case GT => org.objectweb.asm.Opcodes.IF_ICMPGT
