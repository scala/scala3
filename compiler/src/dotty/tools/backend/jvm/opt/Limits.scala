package dotty.tools.backend.jvm.opt

import org.objectweb.asm.tree.MethodNode

/**
 * See the doc comment on package object `analysis` for a discussion on performance.
 */
object Limits:
  // jvm limit is 65535 for both number of instructions and number of locals
  // these numbers are set empirically... for instance, one method with 150 locals and a little over 3k instrs can take ~30s to optimize on a laptop

  def sizeOKForLocalOpt(method: MethodNode): Boolean =
    MethodMax.maxLocals(method) < 500 && method.instructions.size < 3000

  // We'd like methods to still be small enough for local optimizations after inlining
  def sizeOKForInlining(method: MethodNode): Boolean =
    MethodMax.maxLocals(method) < 400 && method.instructions.size < 2000