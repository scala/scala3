package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName

import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.Map
import scala.collection.mutable
import scala.tools.asm
import scala.tools.asm.Handle
import scala.tools.asm.tree.{InvokeDynamicInsnNode, MethodNode}

final class IndyLambdaImplTracker {
  private val indyLambdaImplMethods =
    new ConcurrentHashMap[InternalName, mutable.Map[MethodNode, Map[InvokeDynamicInsnNode, asm.Handle]]]

  private def onMethods[T](hostClass: InternalName)(action: mutable.Map[MethodNode, Map[InvokeDynamicInsnNode, asm.Handle]] => T): T = {
    val methods = indyLambdaImplMethods.computeIfAbsent(hostClass, _ => mutable.Map.empty)
    methods.synchronized(action(methods))
  }

  def add(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode, handle: asm.Handle): Unit = {
    onMethods(hostClass)(_.updateWith(method)(v => v.map(_.updated(indy, handle))))
  }

  def remove(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode): Unit = {
    onMethods(hostClass)(_.updateWith(method)(v => v.map(_.removed(indy))))
  }

  def reset(hostClass: InternalName, method: MethodNode, values: Map[InvokeDynamicInsnNode, Handle]): Unit = {
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
  def get(hostClass: InternalName, method: MethodNode): Map[InvokeDynamicInsnNode, Handle] = {
    onMethods(hostClass)(ms => ms.getOrElseUpdate(method, Map.empty))
  }
}
