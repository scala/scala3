package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.analysis.InitialProducer

import scala.jdk.CollectionConverters.*
import java.io.{PrintWriter, StringWriter}
import scala.tools.asm.tree.{AbstractInsnNode, ClassNode, InsnList, MethodNode}
import scala.tools.asm.util.{ASMifier, Textifier, TraceClassVisitor, TraceMethodVisitor}

object LogUtils {
  /**
   * Returns a human-readable representation of the code in the mnode MethodNode.
   */
  def textify(mnode: MethodNode): String = {
    val trace = new TraceClassVisitor(new PrintWriter(new StringWriter))
    mnode.accept(trace)
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    trace.p.print(pw)
    sw.toString
  }

  /**
   * Returns a human-readable representation of the given instruction.
   */
  def textify(insn: AbstractInsnNode): String = insn match {
    case _: InitialProducer =>
      insn.toString
    case _ =>
      val trace = new TraceMethodVisitor(new Textifier)
      insn.accept(trace)
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      trace.p.print(pw)
      sw.toString.trim
  }

  /**
   * Returns a human-readable representation of the given instruction sequence.
   */
  def textify(insns: Iterator[AbstractInsnNode]): String = {
    val trace = new TraceMethodVisitor(new Textifier)
    insns.foreach(_.accept(trace))
    val sw: StringWriter = new StringWriter
    val pw: PrintWriter = new PrintWriter(sw)
    trace.p.print(pw)
    sw.toString.trim
  }

  /**
   * Returns a human-readable representation of the given instruction sequence.
   */
  def textify(insns: InsnList): String = textify(insns.iterator.asScala)

}
