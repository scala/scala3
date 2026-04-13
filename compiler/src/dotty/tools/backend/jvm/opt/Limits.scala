package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BackendUtils

import scala.tools.asm.tree.MethodNode

/**
 * See the doc comment on package object `analysis` for a discussion on performance.
 */
object Limits:
  // jvm limit is 65535 for both number of instructions and number of locals
  // these numbers are set empirically... for instance, one method with 150 locals and a little over 3k instrs can take ~30s to optimize on a laptop
  def sizeOKForAliasing(method: MethodNode): Boolean = MethodMax.maxLocals(method) < 300 && method.instructions.size < 3000
  def sizeOKForNullness(method: MethodNode): Boolean = MethodMax.maxLocals(method) < 300 && method.instructions.size < 3000
  def sizeOKForBasicValue(method: MethodNode): Boolean = MethodMax.maxLocals(method) < 600 && method.instructions.size < 3000
  def sizeOKForSourceValue(method: MethodNode): Boolean = MethodMax.maxLocals(method) < 500 && method.instructions.size < 2500
