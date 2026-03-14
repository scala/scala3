package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BCodeUtils
import dotty.tools.backend.jvm.analysis.InstructionStackEffect

import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{AbstractInsnNode, IincInsnNode, JumpInsnNode, LookupSwitchInsnNode, MethodNode, TableSwitchInsnNode, VarInsnNode}

object MethodMax {

  def maxLocals(method: MethodNode): Int = {
    computeMaxLocalsMaxStack(method)
    method.maxLocals
  }

  def maxStack(method: MethodNode): Int = {
    computeMaxLocalsMaxStack(method)
    method.maxStack
  }

  /**
   * In order to run an Analyzer, the maxLocals / maxStack fields need to be available. The ASM
   * framework only computes these values during bytecode generation.
   *
   * NOTE 1: as explained in the `analysis` package object, the maxStack value used by the Analyzer
   * may be smaller than the correct maxStack value in the classfile (Analyzers only use a single
   * slot for long / double values). The maxStack computed here are correct for running an analyzer,
   * but not for writing in the classfile. We let the ClassWriter recompute max's.
   *
   * NOTE 2: the maxStack value computed here may be larger than the smallest correct value
   * that would allow running an analyzer, see `InstructionStackEffect.forAsmAnalysis` and
   * `InstructionStackEffect.maxStackGrowth`.
   *
   * NOTE 3: the implementation doesn't look at instructions that cannot be reached, it computes
   * the max local / stack size in the reachable code. These max's work just fine for running an
   * Analyzer: its implementation also skips over unreachable code in the same way.
   */
  private def computeMaxLocalsMaxStack(method: MethodNode): Unit = {
    if (BCodeUtils.isAbstractMethod(method) || BCodeUtils.isNativeMethod(method)) {
      method.maxLocals = 0
      method.maxStack = 0
    } else if (!isMaxsComputed(method)) {
      val size = method.instructions.size

      var maxLocals = BCodeUtils.parametersSize(method)
      var maxStack = 0

      // queue of instruction indices where analysis should start
      var queue = new Array[Int](8)
      var top = -1

      def enq(i: Int): Unit = {
        if (top == queue.length - 1) {
          val nq = new Array[Int](queue.length * 2)
          Array.copy(queue, 0, nq, 0, queue.length)
          queue = nq
        }
        top += 1
        queue(top) = i
      }

      def deq(): Int = {
        val r = queue(top)
        top -= 1
        r
      }

      // for each instruction in the queue, contains the stack height at this instruction.
      // once an instruction has been treated, contains -1 to prevent re-enqueuing
      val stackHeights = new Array[Int](size)

      def enqInsn(insn: AbstractInsnNode, height: Int): Unit = {
        enqInsnIndex(method.instructions.indexOf(insn), height)
      }

      def enqInsnIndex(insnIndex: Int, height: Int): Unit = {
        if (insnIndex < size && stackHeights(insnIndex) != -1) {
          stackHeights(insnIndex) = height
          enq(insnIndex)
        }
      }

      val tcbIt = method.tryCatchBlocks.iterator
      while (tcbIt.hasNext) {
        val tcb = tcbIt.next()
        enqInsn(tcb.handler, 1)
        if (maxStack == 0) maxStack = 1
      }

      /* Subroutines are jumps, historically used for `finally` (https://www.artima.com/underthehood/finally.html)
       *   - JSR pushes the return address (next instruction) on the stack and jumps to a label
       *   - The subroutine typically saves the address to a local variable (ASTORE x)
       *   - The subroutine typically jumps back to the return address using `RET x`, where `x` is the local variable
       *
       * However, the JVM spec does not require subroutines to `RET x` to their caller, they could return back to an
       * outer subroutine caller (nested subroutines), or `RETURN`, or use a static jump. Static analysis of subroutines
       * is therefore complex (https://www21.in.tum.de/~kleing/papers/KleinW-TPHOLS03.pdf).
       *
       * The asm.Analyzer however makes the assumption that subroutines only occur in the shape emitted by early
       * javac, i.e., `RET` always returns to the next enclosing caller. So we do that as well.
       */

      enq(0)
      while (top != -1) {
        val insnIndex = deq()
        val insn = method.instructions.get(insnIndex)
        val initHeight = stackHeights(insnIndex)
        stackHeights(insnIndex) = -1 // prevent i from being enqueued again

        if (insn.getOpcode == -1) { // frames, labels, line numbers
          enqInsnIndex(insnIndex + 1, initHeight)
        } else {
          val stackGrowth = InstructionStackEffect.maxStackGrowth(insn)
          val heightAfter = initHeight + stackGrowth
          if (heightAfter > maxStack) maxStack = heightAfter

          // update maxLocals
          insn match {
            case v: VarInsnNode =>
              val longSize = if (BCodeUtils.isSize2LoadOrStore(v.getOpcode)) 1 else 0
              maxLocals = math.max(maxLocals, v.`var` + longSize + 1) // + 1 because local numbers are 0-based

            case i: IincInsnNode =>
              maxLocals = math.max(maxLocals, i.`var` + 1)

            case _ =>
          }

          insn match {
            case j: JumpInsnNode =>
              val opc = j.getOpcode
              if (opc == Opcodes.JSR) {
                val jsrTargetHeight = heightAfter + 1
                if (jsrTargetHeight > maxStack) maxStack = jsrTargetHeight
                enqInsn(j.label, jsrTargetHeight)
                enqInsnIndex(insnIndex + 1, heightAfter) // see subroutine shape assumption above
              } else {
                enqInsn(j.label, heightAfter)
                if (opc != Opcodes.GOTO) enqInsnIndex(insnIndex + 1, heightAfter) // jump is conditional, so the successor is also a possible control flow target
              }

            case l: LookupSwitchInsnNode =>
              var j = 0
              while (j < l.labels.size) {
                enqInsn(l.labels.get(j), heightAfter);
                j += 1
              }
              enqInsn(l.dflt, heightAfter)

            case t: TableSwitchInsnNode =>
              var j = 0
              while (j < t.labels.size) {
                enqInsn(t.labels.get(j), heightAfter);
                j += 1
              }
              enqInsn(t.dflt, heightAfter)

            case r: VarInsnNode if r.getOpcode == Opcodes.RET =>
            // the target is already enqueued, see subroutine shape assumption above

            case _ =>
              if (insn.getOpcode != Opcodes.ATHROW && !BCodeUtils.isReturn(insn))
                enqInsnIndex(insnIndex + 1, heightAfter)
          }
        }
      }

      method.maxLocals = maxLocals
      method.maxStack = maxStack

      setMaxsComputed(method)
    }
  }

  /**
   * A pseudo-flag, added MethodNodes whose maxLocals / maxStack are computed. This allows invoking
   * `computeMaxLocalsMaxStack` whenever running an analyzer but performing the actual computation
   * only when necessary.
   *
   * The largest JVM flag (as of JDK 8) is ACC_MANDATED (0x8000), however the asm framework uses
   * the same trick and defines some pseudo flags
   *   - ACC_DEPRECATED = 0x20000
   *   - ACC_SYNTHETIC_ATTRIBUTE = 0x40000
   *   - ACC_CONSTRUCTOR = 0x80000
   *
   * I haven't seen the value picked here in use anywhere. We make sure to remove the flag when
   * it's no longer needed.
   */
  private val ACC_MAXS_COMPUTED = 0x1000000
  private def isMaxsComputed(method: MethodNode): Boolean = (method.access & ACC_MAXS_COMPUTED) != 0
  private def setMaxsComputed(method: MethodNode): Unit = method.access |= ACC_MAXS_COMPUTED
}
