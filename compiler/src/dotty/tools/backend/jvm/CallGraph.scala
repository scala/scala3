package dotty.tools.backend.jvm

import dotty.tools.backend.jvm.opt.{Callsite, ClosureInstantiation}
import dotty.tools.dotc.util.SourcePosition

import scala.tools.asm.tree.{InvokeDynamicInsnNode, MethodInsnNode, MethodNode}

/**
 * Basics of a call graph: only the parts needed to emit code and do local optimizations.
 */
abstract class CallGraph {
  def recordCallsitePosition(m: MethodInsnNode, pos: SourcePosition): Unit
  def removeCallsite(invocation: MethodInsnNode, methodNode: MethodNode): Option[Callsite]
  def removeClosureInstantiation(indy: InvokeDynamicInsnNode, methodNode: MethodNode): Unit
}

/**
 * Disabled call graph. Does not record any data.
 */
object DisabledCallGraph extends CallGraph {
  override def recordCallsitePosition(m: MethodInsnNode, pos: SourcePosition): Unit =
    ()

  override def removeCallsite(invocation: MethodInsnNode, methodNode: MethodNode): Option[Callsite] =
    None

  override def removeClosureInstantiation(indy: InvokeDynamicInsnNode, methodNode: MethodNode): Unit =
    ()
}
