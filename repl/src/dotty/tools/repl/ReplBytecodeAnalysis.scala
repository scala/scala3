package dotty.tools
package repl

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.tools.asm.*
import scala.tools.asm.Opcodes.*
import scala.tools.asm.tree.*
import scala.util.control.NonFatal

object ReplBytecodeAnalysis:

  def classBytes(clazz: Class[?]): Array[Byte] | Null =
    val resourceName = clazz.getName.replace('.', '/') + ".class"
    val loader = clazz.getClassLoader
    val stream =
      if loader == null then ClassLoader.getSystemResourceAsStream(resourceName)
      else loader.getResourceAsStream(resourceName)
    if stream == null then null
    else
      try stream.readAllBytes()
      finally stream.close()

  def isScalaRunTimeProductToString(clazz: Class[?]): Boolean =

    def isScalaRunTimeModule(insn: AbstractInsnNode): Boolean = insn match
      case insn: FieldInsnNode =>
        insn.getOpcode == GETSTATIC
          && insn.owner == "scala/runtime/ScalaRunTime$"
          && insn.name == "MODULE$"
      case _ => false

    def isLoadThis(insn: AbstractInsnNode): Boolean = insn match
      case insn: VarInsnNode => insn.getOpcode == ALOAD && insn.`var` == 0
      case _ => false

    def isScalaRunTimeToString(insn: AbstractInsnNode): Boolean = insn match
      case insn: MethodInsnNode =>
        (insn.getOpcode == INVOKEVIRTUAL || insn.getOpcode == INVOKESTATIC)
          && (insn.owner == "scala/runtime/ScalaRunTime$" || insn.owner == "scala/runtime/ScalaRunTime")
          && insn.name == "_toString"
          && insn.desc == "(Lscala/Product;)Ljava/lang/String;"
      case _ => false

    def isReturn(insn: AbstractInsnNode): Boolean =
      insn.getOpcode == ARETURN

    def isSynthesizedToString(method: MethodNode): Boolean =
      method.instructions.iterator.asScala.filter(_.getOpcode >= 0).toList match
        case List(module, loadThis, call, ret) =>
          isScalaRunTimeModule(module) && isLoadThis(loadThis) && isScalaRunTimeToString(call) && isReturn(ret)
        case List(loadThis, call, ret) =>
          isLoadThis(loadThis) && isScalaRunTimeToString(call) && isReturn(ret)
        case _ => false

    try
      val bytes = classBytes(clazz)
      if bytes == null then false
      else
        val classNode = ClassNode()
        ClassReader(bytes).accept(classNode, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
        classNode.methods.asScala.exists: method =>
          method.name == "toString"
            && method.desc == "()Ljava/lang/String;"
            && isSynthesizedToString(method)
    catch case NonFatal(_) => false

end ReplBytecodeAnalysis
