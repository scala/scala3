/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.dotc
package backend.jvm

object icodes {

  /** This class represents a test operation. */
  sealed abstract class TestOp {

    /** Returns the negation of this operation. */
    def negate(): TestOp

    /** Returns a string representation of this operation. */
    override def toString(): String

    /** used only from GenASM */
    def opcodeIF(): Int

    /** used only from GenASM */
    def opcodeIFICMP(): Int

  }

  /** An equality test */
  case object EQ extends TestOp {
    def negate() = NE
    override def toString() = "EQ"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFEQ
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPEQ
  }

  /** A non-equality test */
  case object NE extends TestOp {
    def negate() = EQ
    override def toString() = "NE"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFNE
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPNE
  }

  /** A less-than test */
  case object LT extends TestOp {
    def negate() = GE
    override def toString() = "LT"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFLT
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPLT
  }

  /** A greater-than-or-equal test */
  case object GE extends TestOp {
    def negate() = LT
    override def toString() = "GE"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFGE
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPGE
  }

  /** A less-than-or-equal test */
  case object LE extends TestOp {
    def negate() = GT
    override def toString() = "LE"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFLE
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPLE
  }

  /** A greater-than test */
  case object GT extends TestOp {
    def negate() = LE
    override def toString() = "GT"
    override def opcodeIF()     = dotty.tools.asm.Opcodes.IFGT
    override def opcodeIFICMP() = dotty.tools.asm.Opcodes.IF_ICMPGT
  }

  /** This class represents an arithmetic operation. */
  class ArithmeticOp {

    /** Returns a string representation of this operation. */
    override def toString(): String = this match {
      case ADD => "ADD"
      case SUB => "SUB"
      case MUL => "MUL"
      case DIV => "DIV"
      case REM => "REM"
      case NOT => "NOT"
      case _   => throw new RuntimeException("ArithmeticOp unknown case")
    }
  }

  /** An arithmetic addition operation */
  case object ADD extends ArithmeticOp

  /** An arithmetic subtraction operation */
  case object SUB extends ArithmeticOp

  /** An arithmetic multiplication operation */
  case object MUL extends ArithmeticOp

  /** An arithmetic division operation */
  case object DIV extends ArithmeticOp

  /** An arithmetic remainder operation */
  case object REM extends ArithmeticOp

  /** Bitwise negation. */
  case object NOT extends ArithmeticOp

  object opcodes {

    /** This class represents a method invocation style. */
    sealed abstract class InvokeStyle {
      /** Is this a dynamic method call? */
      def isDynamic: Boolean = false

      /** Is this a static method call? */
      def isStatic: Boolean = false

      def isSuper: Boolean = false

      /** Is this an instance method call? */
      def hasInstance: Boolean = true

      /** Returns a string representation of this style. */
      override def toString(): String
    }

    /** Virtual calls.
     *  On JVM, translated to either `invokeinterface` or `invokevirtual`.
     */
    case object Dynamic extends InvokeStyle {
      override def isDynamic = true
      override def toString(): String = "dynamic"
    }

    /**
     * Special invoke:
     *   Static(true)  is used for calls to private members, ie `invokespecial` on JVM.
     *   Static(false) is used for calls to class-level instance-less static methods, ie `invokestatic` on JVM.
     */
    case class Static(onInstance: Boolean) extends InvokeStyle {
      override def isStatic    = true
      override def hasInstance = onInstance
      override def toString(): String = {
        if(onInstance) "static-instance"
        else           "static-class"
      }
    }

    /** Call through super[mix].
     *  On JVM, translated to `invokespecial`.
     */
    case class SuperCall(mix: dotc.core.Names.TermName) extends InvokeStyle {
      override def isSuper = true
      override def toString(): String = { "super(" + mix + ")" }
    }
  }

}
