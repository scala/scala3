package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.tools.asm
import scala.tools.asm.Handle
import scala.tools.asm.tree.{InvokeDynamicInsnNode, MethodNode}

final class IndyLambdaImplTracker {
  private val indyLambdaImplMethods =
    new ConcurrentHashMap[InternalName, mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]]]

  private def onMethods[T](hostClass: InternalName)(action: mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]] => T): T = {
    val methods = indyLambdaImplMethods.computeIfAbsent(hostClass, _ => mutable.Map.empty)
    methods.synchronized(action(methods))
  }

  def add(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode, handle: asm.Handle): Unit = {
    onMethods(hostClass)(_.getOrElseUpdate(method, mutable.Map.empty)(indy) = handle)
  }

  def remove(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode): Unit = {
    onMethods(hostClass)(_.get(method).foreach(_.remove(indy)))
  }

  def reset(hostClass: InternalName, method: MethodNode, values: mutable.Map[InvokeDynamicInsnNode, Handle]): Unit = {
    onMethods(hostClass)(ms => {
      if values.isEmpty then
        ms.remove(method)
      else
        ms(method) = values
    })
  }

  /**
   * The methods used as lambda bodies for IndyLambda instructions within `method` of `hostClass`.
   */
  def get(hostClass: InternalName, method: MethodNode): mutable.Map[InvokeDynamicInsnNode, Handle] = {
    onMethods(hostClass)(ms => ms.getOrElseUpdate(method, mutable.Map.empty))
  }
}
