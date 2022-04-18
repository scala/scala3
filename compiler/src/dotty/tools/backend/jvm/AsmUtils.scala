package dotty.tools
package backend
package jvm

import scala.language.unsafeNulls

import scala.tools.asm.tree.{AbstractInsnNode}
import java.io.PrintWriter
import scala.tools.asm.util.{TraceClassVisitor, TraceMethodVisitor, Textifier}
import scala.tools.asm.ClassReader

object AsmUtils {

  /**
   * Print the bytecode of methods generated by GenBCode to the standard output. Only methods
   * whose name contains `traceMethodPattern` are traced.
   */
  final val traceMethodEnabled = sys.env.contains("printBCODE")
  final val traceMethodPattern = sys.env.getOrElse("printBCODE", "")

  /**
   * Print the bytecode of classes generated by GenBCode to the standard output.
   */
  inline val traceClassEnabled = false
  inline val traceClassPattern = ""

  /**
   * Print the bytedcode of classes as they are serialized by the ASM library. The serialization
   * performed by `asm.ClassWriter` can change the code generated by GenBCode. For example, it
   * introduces stack map frames, it computes the maximal stack sizes, and it replaces dead
   * code by NOPs (see also https://github.com/scala/scala/pull/3726#issuecomment-42861780).
   */
  inline val traceSerializedClassEnabled = false
  inline val traceSerializedClassPattern = ""

  def traceMethod(mnode: MethodNode1): Unit = {
    println(s"Bytecode for method ${mnode.name}")
    val p = new Textifier
    val tracer = new TraceMethodVisitor(p)
    mnode.accept(tracer)
    val w = new PrintWriter(System.out)
    p.print(w)
    w.flush()
  }

  def traceClass(cnode: ClassNode1): Unit = {
    println(s"Bytecode for class ${cnode.name}")
    val w = new PrintWriter(System.out)
    cnode.accept(new TraceClassVisitor(w))
    w.flush()
  }

  def traceClass(bytes: Array[Byte]): Unit = traceClass(readClass(bytes))

  def readClass(bytes: Array[Byte]): ClassNode1 = {
    val node = new ClassNode1()
    new ClassReader(bytes).accept(node, 0)
    node
  }

  def instructionString(instruction: AbstractInsnNode): String = instruction.getOpcode match {
    case -1 => instruction.toString
    case op => scala.tools.asm.util.Printer.OPCODES(op)
  }
}
