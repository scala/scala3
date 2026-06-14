package dotty.tools
package backend
package jvm

import dotty.tools.backend.jvm.opt.CallGraph
import scala.tools.asm
import scala.annotation.tailrec
import scala.tools.asm.tree.MethodInsnNode
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.NoSourcePosition

/*
 *  A high-level facade to the ASM API for bytecode generation.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
trait BCodeIdiomatic(callGraph: Option[CallGraph]) {
  private val debugLevel = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames
  final val emitSource = debugLevel >= 1
  final val emitLines = debugLevel >= 2
  final val emitVars = debugLevel >= 3

  private def recordCallsitePosition(m: MethodInsnNode, pos: Positioned | Null)(using Context): Unit =
     callGraph.foreach(_.recordCallsitePosition(m, pos match {
      case p: Positioned => p.sourcePos
      case null => NoSourcePosition
    }))

  abstract class JCodeMethodN {

    def jmethod: asm.tree.MethodNode

    import asm.Opcodes

    /*
     * can-multi-thread
     */
    final def genPrimitiveNot(kind: BType): Unit =
      if (kind.isIntSizedType) {
        jmethod.visitInsn(Opcodes.ICONST_M1)
        jmethod.visitInsn(Opcodes.IXOR)
      } else if (kind == LONG) {
        jmethod.visitLdcInsn(java.lang.Long.valueOf(-1))
        jmethod.visitInsn(Opcodes.LXOR)
      } else {
        throw new AssertionError(s"Impossible to negate an $kind")
      }
    end genPrimitiveNot

    /*
     * can-multi-thread
     */
    final def genPrimitiveLogical(op: /* LogicalOp */ Int, kind: BType): Unit = {

      import ScalaPrimitivesOps.{ AND, OR, XOR }

      ((op, kind): @unchecked) match {
        case (AND, LONG) => jmethod.visitInsn(Opcodes.LAND)
        case (AND, INT)  => jmethod.visitInsn(Opcodes.IAND)
        case (AND, _)    =>
          jmethod.visitInsn(Opcodes.IAND)
          if (kind != BOOL) { emitT2T(INT, kind) }

        case (OR, LONG) => jmethod.visitInsn(Opcodes.LOR)
        case (OR, INT)  => jmethod.visitInsn(Opcodes.IOR)
        case (OR, _) =>
          jmethod.visitInsn(Opcodes.IOR)
          if (kind != BOOL) { emitT2T(INT, kind) }

        case (XOR, LONG) => jmethod.visitInsn(Opcodes.LXOR)
        case (XOR, INT)  => jmethod.visitInsn(Opcodes.IXOR)
        case (XOR, _) =>
          jmethod.visitInsn(Opcodes.IXOR)
          if (kind != BOOL) { emitT2T(INT, kind) }
      }

    } // end of method genPrimitiveLogical()

    /*
     * can-multi-thread
     */
    final def genPrimitiveShift(op: /* ShiftOp */ Int, kind: BType): Unit = {

      import ScalaPrimitivesOps.{ LSL, ASR, LSR }

      ((op, kind): @unchecked) match {
        case (LSL, LONG) => jmethod.visitInsn(Opcodes.LSHL)
        case (LSL, INT)  => jmethod.visitInsn(Opcodes.ISHL)
        case (LSL, _) =>
          jmethod.visitInsn(Opcodes.ISHL)
          emitT2T(INT, kind)

        case (ASR, LONG) => jmethod.visitInsn(Opcodes.LSHR)
        case (ASR, INT)  => jmethod.visitInsn(Opcodes.ISHR)
        case (ASR, _) =>
          jmethod.visitInsn(Opcodes.ISHR)
          emitT2T(INT, kind)

        case (LSR, LONG) => jmethod.visitInsn(Opcodes.LUSHR)
        case (LSR, INT)  => jmethod.visitInsn(Opcodes.IUSHR)
        case (LSR, _) =>
          jmethod.visitInsn(Opcodes.IUSHR)
          emitT2T(INT, kind)
      }

    } // end of method genPrimitiveShift()

    /* Concatenate top N arguments on the stack with `StringConcatFactory#makeConcatWithConstants`
     * (only works for JDK 9+)
     *
     * can-multi-thread
     */
    final def genIndyStringConcat(
      recipe: String,
      argTypes: Seq[asm.Type],
      constants: Seq[String],
      bTypes: KnownBTypes
    ): Unit = {
      jmethod.visitInvokeDynamicInsn(
        "makeConcatWithConstants",
        asm.Type.getMethodDescriptor(bTypes.StringRef.toASMType, argTypes*),
        bTypes.jliStringConcatFactoryMakeConcatWithConstantsHandle,
        (recipe +: constants)*
      )
    }

    /*
     * Emits one or more conversion instructions based on the types given as arguments.
     *
     * @param from The type of the value to be converted into another type.
     * @param to   The type the value will be converted into.
     *
     * can-multi-thread
     */
    final def emitT2T(from: BType, to: BType): Unit =
      assert(
        from.isNonVoidPrimitiveType && to.isNonVoidPrimitiveType,
        s"Cannot emit primitive conversion from $from to $to"
      )

      def pickOne(opcs: Array[Int]): Unit = { // TODO index on to.sort
        val chosen = (to: @unchecked) match {
          case BYTE   => opcs(0)
          case SHORT  => opcs(1)
          case CHAR   => opcs(2)
          case INT    => opcs(3)
          case LONG   => opcs(4)
          case FLOAT  => opcs(5)
          case DOUBLE => opcs(6)
        }
        if (chosen != -1) { jmethod.visitInsn(chosen) }
      }

      if (from == to) { return }
      // the only conversion involving BOOL that is allowed is (BOOL -> BOOL)
      assert(from != BOOL && to != BOOL, s"inconvertible types : $from -> $to")

      // we're done with BOOL already
      from match {
        // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"

        case BYTE  => pickOne(JCodeMethodN.fromByteT2T)
        case SHORT => pickOne(JCodeMethodN.fromShortT2T)
        case CHAR  => pickOne(JCodeMethodN.fromCharT2T)
        case INT   => pickOne(JCodeMethodN.fromIntT2T)

        case FLOAT  =>
          import asm.Opcodes.{ F2L, F2D, F2I }
          to match {
            case LONG    => jmethod.visitInsn(F2L)
            case DOUBLE  => jmethod.visitInsn(F2D)
            case _       => jmethod.visitInsn(F2I); emitT2T(INT, to)
          }

        case LONG   =>
          import asm.Opcodes.{ L2F, L2D, L2I }
          to match {
            case FLOAT   => jmethod.visitInsn(L2F)
            case DOUBLE  => jmethod.visitInsn(L2D)
            case _       => jmethod.visitInsn(L2I); emitT2T(INT, to)
          }

        case DOUBLE =>
          import asm.Opcodes.{ D2L, D2F, D2I }
          to match {
            case FLOAT   => jmethod.visitInsn(D2F)
            case LONG    => jmethod.visitInsn(D2L)
            case _       => jmethod.visitInsn(D2I); emitT2T(INT, to)
          }

        case _ => throw new IllegalArgumentException("Unsupported from type for T2T: " + from)
      }
    end emitT2T

    // can-multi-thread
    final def boolconst(b: Boolean): Unit = { iconst(if (b) 1 else 0) }

    // can-multi-thread
    final def iconst(cst: Int): Unit = {
      if (cst >= -1 && cst <= 5) {
        jmethod.visitInsn(Opcodes.ICONST_0 + cst)
      } else if (cst >= java.lang.Byte.MIN_VALUE && cst <= java.lang.Byte.MAX_VALUE) {
        jmethod.visitIntInsn(Opcodes.BIPUSH, cst)
      } else if (cst >= java.lang.Short.MIN_VALUE && cst <= java.lang.Short.MAX_VALUE) {
        jmethod.visitIntInsn(Opcodes.SIPUSH, cst)
      } else {
        jmethod.visitLdcInsn(Integer.valueOf(cst))
      }
    }

    // can-multi-thread
    final def lconst(cst: Long): Unit = {
      if (cst == 0L || cst == 1L) {
        jmethod.visitInsn(Opcodes.LCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(java.lang.Long.valueOf(cst))
      }
    }

    // can-multi-thread
    final def fconst(cst: Float): Unit = {
      val bits: Int = java.lang.Float.floatToIntBits(cst)
      if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
        jmethod.visitInsn(Opcodes.FCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(java.lang.Float.valueOf(cst))
      }
    }

    // can-multi-thread
    final def dconst(cst: Double): Unit = {
      val bits: Long = java.lang.Double.doubleToLongBits(cst)
      if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
        jmethod.visitInsn(Opcodes.DCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(java.lang.Double.valueOf(cst))
      }
    }

    final def nullconst(): Unit =
      jmethod.visitInsn(Opcodes.ACONST_NULL)

    // can-multi-thread
    final def newarray(elem: BType): Unit = {
      elem match {
        case c: RefBType =>
          /* phantom type at play in `Array(null)`, SI-1513. On the other hand, Array(()) has element type `scala.runtime.BoxedUnit` which isObject. */
          jmethod.visitTypeInsn(Opcodes.ANEWARRAY, c.classOrArrayType)
        case _ =>
          assert(elem.isNonVoidPrimitiveType)
          val rand = {
            elem match {
              case BOOL   => Opcodes.T_BOOLEAN
              case BYTE   => Opcodes.T_BYTE
              case SHORT  => Opcodes.T_SHORT
              case CHAR   => Opcodes.T_CHAR
              case INT    => Opcodes.T_INT
              case LONG   => Opcodes.T_LONG
              case FLOAT  => Opcodes.T_FLOAT
              case DOUBLE => Opcodes.T_DOUBLE
              case _ => throw new IllegalArgumentException("Invalid array element type: " + elem)
            }
          }
          jmethod.visitIntInsn(Opcodes.NEWARRAY, rand)
      }
    }


    final def load( idx: Int, tk: BType): Unit = { emitVarInsn(Opcodes.ILOAD,  idx, tk) } // can-multi-thread
    final def store(idx: Int, tk: BType): Unit = { emitVarInsn(Opcodes.ISTORE, idx, tk) } // can-multi-thread
    final def iinc( idx: Int, increment: Int): Unit = jmethod.visitIincInsn(idx, increment) // can-multi-thread

    final def aload( tk: BType): Unit = { emitTypeBased(JCodeMethodN.aloadOpcodes,  tk) } // can-multi-thread
    final def astore(tk: BType): Unit = { emitTypeBased(JCodeMethodN.astoreOpcodes, tk) } // can-multi-thread

    final def neg(tk: BType): Unit = { emitPrimitive(JCodeMethodN.negOpcodes, tk) } // can-multi-thread
    final def add(tk: BType): Unit = { emitPrimitive(JCodeMethodN.addOpcodes, tk) } // can-multi-thread
    final def sub(tk: BType): Unit = { emitPrimitive(JCodeMethodN.subOpcodes, tk) } // can-multi-thread
    final def mul(tk: BType): Unit = { emitPrimitive(JCodeMethodN.mulOpcodes, tk) } // can-multi-thread
    final def div(tk: BType): Unit = { emitPrimitive(JCodeMethodN.divOpcodes, tk) } // can-multi-thread
    final def rem(tk: BType): Unit = { emitPrimitive(JCodeMethodN.remOpcodes, tk) } // can-multi-thread

    // can-multi-thread
    final def invokespecial(owner: String, name: String, desc: String, itf: Boolean, pos: Positioned | Null)(using Context): Unit = {
      emitInvoke(Opcodes.INVOKESPECIAL, owner, name, desc, itf, pos)
    }
    // can-multi-thread
    final def invokestatic(owner: String, name: String, desc: String, itf: Boolean, pos: Positioned | Null)(using Context): Unit = {
      emitInvoke(Opcodes.INVOKESTATIC, owner, name, desc, itf, pos)
    }
    // can-multi-thread
    final def invokeinterface(owner: String, name: String, desc: String, pos: Positioned | Null)(using Context): Unit = {
      emitInvoke(Opcodes.INVOKEINTERFACE, owner, name, desc, itf = true, pos)
    }
    // can-multi-thread
    final def invokevirtual(owner: String, name: String, desc: String, pos: Positioned | Null)(using Context): Unit = {
      emitInvoke(Opcodes.INVOKEVIRTUAL, owner, name, desc, itf = false, pos)
    }

    private def emitInvoke(opcode: Int, owner: String, name: String, desc: String, itf: Boolean, pos: Positioned | Null)(using Context): Unit = {
      val node = new MethodInsnNode(opcode, owner, name, desc, itf)
      jmethod.instructions.add(node)
      recordCallsitePosition(node, pos)
    }


    // can-multi-thread
    final def goTo(label: asm.Label): Unit = { jmethod.visitJumpInsn(Opcodes.GOTO, label) }
    // can-multi-thread
    final def emitIF(cond: TestOp, label: asm.Label): Unit =      { jmethod.visitJumpInsn(cond.opcodeIF(),     label) }
    // can-multi-thread
    final def emitIF_ICMP(cond: TestOp, label: asm.Label): Unit = { jmethod.visitJumpInsn(cond.opcodeIFICMP(), label) }
    // can-multi-thread
    final def emitIF_ACMP(cond: TestOp, label: asm.Label): Unit = {
      assert((cond == TestOp.EQ) || (cond == TestOp.NE), cond)
      val opc = if (cond == TestOp.EQ) Opcodes.IF_ACMPEQ else Opcodes.IF_ACMPNE
      jmethod.visitJumpInsn(opc, label)
    }
    // can-multi-thread
    final def emitIFNONNULL(label: asm.Label): Unit = { jmethod.visitJumpInsn(Opcodes.IFNONNULL, label) }
    // can-multi-thread
    final def emitIFNULL   (label: asm.Label): Unit = { jmethod.visitJumpInsn(Opcodes.IFNULL,    label) }

    // can-multi-thread
    final def emitRETURN(tk: BType): Unit = {
      if (tk == UNIT) { jmethod.visitInsn(Opcodes.RETURN) }
      else            { emitTypeBased(JCodeMethodN.returnOpcodes, tk)      }
    }

    /* Emits one of tableswitch or lookupswitch.
     *
     * can-multi-thread
     */
    final def emitSWITCH(unsortedKeysAndBranches: List[(Int, asm.Label)], defaultBranch: asm.Label, minDensity: Double): Unit = {
      val keysAndBranches = unsortedKeysAndBranches.sortBy(_._1)

      // check for duplicate keys to avoid "VerifyError: unsorted lookupswitch" (SI-6011)
      @tailrec
      def scan(lst: List[(Int, asm.Label)], prev: Int): Unit = lst match {
        case (n, _) :: tl if n == prev => throw new AssertionError("duplicate keys in SWITCH, can't pick arbitrarily one of them to evict, see SI-6011.")
        case (n, _) :: tl => scan(tl, n)
        case _ => ()
      }

      // For empty keys, it makes sense emitting LOOKUPSWITCH with defaultBranch only.
      // Similar to what javac emits for a switch statement consisting only of a default case.
      keysAndBranches match
        case Nil =>
          jmethod.visitLookupSwitchInsn(defaultBranch, Array.empty[Int], Array.empty[asm.Label])
          return
        case (n, _) :: tl =>
          scan(tl, n)

      val keyMin = keysAndBranches.head._1
      val keyMax = keysAndBranches.last._1

      val isDenseEnough: Boolean = {
        /* Calculate in long to guard against overflow. TODO what overflow? */
        val keyRangeD: Double = (keyMax.asInstanceOf[Long] - keyMin + 1).asInstanceOf[Double]
        val klenD:     Double = keysAndBranches.length
        val kdensity:  Double = klenD / keyRangeD

        kdensity >= minDensity
      }

      if (isDenseEnough) {
        // use a table in which holes are filled with defaultBranch.
        val keyRange    = keyMax - keyMin + 1
        val newBranches = new Array[asm.Label](keyRange)
        var remainingKeysAndBranches = keysAndBranches
        var i = 0
        while (i < keyRange) {
          val key = keyMin + i
          if (remainingKeysAndBranches.head._1 == key) {
            newBranches(i) = remainingKeysAndBranches.head._2
            remainingKeysAndBranches = remainingKeysAndBranches.tail
          } else {
            newBranches(i) = defaultBranch
          }
          i += 1
        }
        assert(remainingKeysAndBranches.isEmpty, "emitSWITCH")
        jmethod.visitTableSwitchInsn(keyMin, keyMax, defaultBranch, newBranches*)
      } else {
        val len = keysAndBranches.length
        val keys = new Array[Int](len)
        val branches = new Array[asm.Label](len)
        var remainingKeysAndBranches = keysAndBranches
        var idx = 0
        while remainingKeysAndBranches.nonEmpty do
          keys(idx) = remainingKeysAndBranches.head._1
          branches(idx) = remainingKeysAndBranches.head._2
          remainingKeysAndBranches = remainingKeysAndBranches.tail
          idx = idx + 1
        jmethod.visitLookupSwitchInsn(defaultBranch, keys, branches)
      }
    }

    // internal helpers -- not part of the public API of `jcode`
    // don't make private otherwise inlining will suffer

    // can-multi-thread
    private final def emitVarInsn(opc: Int, idx: Int, tk: BType): Unit = {
      assert((opc == Opcodes.ILOAD) || (opc == Opcodes.ISTORE), opc)
      jmethod.visitVarInsn(tk.typedOpcode(opc), idx)
    }

    // ---------------- array load and store ----------------

    // can-multi-thread
    private final def emitTypeBased(opcs: Array[Int], tk: BType): Unit = {
      assert(tk != UNIT, tk)
      val opc = {
        if (tk.isRef) { opcs(0) }
        else if (tk.isIntSizedType) {
          (tk: @unchecked) match {
            case BOOL | BYTE     => opcs(1)
            case SHORT           => opcs(2)
            case CHAR            => opcs(3)
            case INT             => opcs(4)
          }
        } else {
          (tk: @unchecked) match {
            case LONG            => opcs(5)
            case FLOAT           => opcs(6)
            case DOUBLE          => opcs(7)
          }
        }
      }
      jmethod.visitInsn(opc)
    }

    // ---------------- primitive operations ----------------

     // can-multi-thread
    private final def emitPrimitive(opcs: Array[Int], tk: BType): Unit = {
      val opc = {
        // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
        tk match {
          case LONG   => opcs(1)
          case FLOAT  => opcs(2)
          case DOUBLE => opcs(3)
          case _      => opcs(0)
        }
      }
      jmethod.visitInsn(opc)
    }

    // can-multi-thread
    final def drop(tk: BType): Unit = { jmethod.visitInsn(if (tk.isWideType) Opcodes.POP2 else Opcodes.POP) }

    // can-multi-thread
    final def dropMany(size: Int): Unit = {
      var s = size
      while s >= 2 do
        jmethod.visitInsn(Opcodes.POP2)
        s -= 2
      if s > 0 then
        jmethod.visitInsn(Opcodes.POP)
    }

    // can-multi-thread
    final def dup(tk: BType): Unit =  { jmethod.visitInsn(if (tk.isWideType) Opcodes.DUP2 else Opcodes.DUP) }

    // ---------------- type checks and casts ----------------

    // can-multi-thread
    final def isInstance(tk: RefBType): Unit = {
      jmethod.visitTypeInsn(Opcodes.INSTANCEOF, tk.classOrArrayType)
    }

    // can-multi-thread
    final def checkCast(tk: RefBType): Unit = {
      // TODO ICode also requires: but that's too much, right? assert(!isBoxedType(tk),     "checkcast on boxed type: " + tk)
      jmethod.visitTypeInsn(Opcodes.CHECKCAST, tk.classOrArrayType)
    }

  } // end of class JCodeMethodN

  /* Constant-valued val-members of JCodeMethodN at the companion object, so as to avoid re-initializing them multiple times. */
  object JCodeMethodN {

    import asm.Opcodes.*

    // ---------------- conversions ----------------

    val fromByteT2T  = { Array( -1,  -1, I2C,  -1, I2L, I2F, I2D) } // do nothing for (BYTE -> SHORT) and for (BYTE -> INT)
    val fromCharT2T  = { Array(I2B, I2S,  -1,  -1, I2L, I2F, I2D) } // for (CHAR  -> INT) do nothing
    val fromShortT2T = { Array(I2B,  -1, I2C,  -1, I2L, I2F, I2D) } // for (SHORT -> INT) do nothing
    val fromIntT2T   = { Array(I2B, I2S, I2C,  -1, I2L, I2F, I2D) }

    // ---------------- array load and store ----------------

    val aloadOpcodes  = { Array(AALOAD,  BALOAD,  SALOAD,  CALOAD,  IALOAD,  LALOAD,  FALOAD,  DALOAD)  }
    val astoreOpcodes = { Array(AASTORE, BASTORE, SASTORE, CASTORE, IASTORE, LASTORE, FASTORE, DASTORE) }
    val returnOpcodes = { Array(ARETURN, IRETURN, IRETURN, IRETURN, IRETURN, LRETURN, FRETURN, DRETURN) }

    // ---------------- primitive operations ----------------

    val negOpcodes: Array[Int] = { Array(INEG, LNEG, FNEG, DNEG) }
    val addOpcodes: Array[Int] = { Array(IADD, LADD, FADD, DADD) }
    val subOpcodes: Array[Int] = { Array(ISUB, LSUB, FSUB, DSUB) }
    val mulOpcodes: Array[Int] = { Array(IMUL, LMUL, FMUL, DMUL) }
    val divOpcodes: Array[Int] = { Array(IDIV, LDIV, FDIV, DDIV) }
    val remOpcodes: Array[Int] = { Array(IREM, LREM, FREM, DREM) }

  } // end of object JCodeMethodN

}
