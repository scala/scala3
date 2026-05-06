package dotty.tools
package repl

import scala.language.unsafeNulls

import scala.tools.asm.*
import scala.tools.asm.Opcodes.*
import scala.tools.asm.tree.*
import scala.jdk.CollectionConverters.*
import java.util.concurrent.atomic.AtomicBoolean

object ReplBytecodeInstrumentation:
  /** Instrument bytecode to add checks to throw an exception if the REPL command is cancelled
   */
  def instrument(originalBytes: Array[Byte]): Array[Byte] =
    try
      val cr = new ClassReader(originalBytes)
      val cw = new ClassWriter(cr, ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
      val instrumenter = new InstrumentClassVisitor(cw)
      cr.accept(instrumenter, ClassReader.EXPAND_FRAMES)
      cw.toByteArray
    catch
      case ex: Exception => originalBytes

  def setStopFlag(classLoader: ClassLoader, b: Boolean): Unit =
    val cancelClassOpt =
      try Some(classLoader.loadClass(classOf[StopRepl].getName))
      catch {
        case _: java.lang.ClassNotFoundException => None
      }
    for(cancelClass <- cancelClassOpt) {
      val setAllStopMethod = cancelClass.getDeclaredMethod("setStop", classOf[Boolean])
      setAllStopMethod.invoke(null, b.asInstanceOf[AnyRef])
    }

  private class InstrumentClassVisitor(cv: ClassVisitor) extends ClassVisitor(ASM9, cv):

    override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String]
    ): MethodVisitor =
      new InstrumentMethodVisitor(super.visitMethod(access, name, descriptor, signature, exceptions))

  /** MethodVisitor that inserts stop checks at backward branches */
  private class InstrumentMethodVisitor(mv: MethodVisitor) extends MethodVisitor(ASM9, mv):
    // Track labels we've seen to identify backward branches
    private val seenLabels = scala.collection.mutable.Set[Label]()

    def addStopCheck() = mv.visitMethodInsn(
      INVOKESTATIC,
      classOf[StopRepl].getName.replace('.', '/'),
      "throwIfReplStopped",
      "()V",
      false
    )

    override def visitCode(): Unit =
      super.visitCode()
      // Insert throwIfReplStopped() call at the start of the method
      // to allow breaking out of deeply recursive methods like fib(99)
      addStopCheck()

    override def visitLabel(label: Label): Unit =
      seenLabels.add(label)
      super.visitLabel(label)

    override def visitJumpInsn(opcode: Int, label: Label): Unit =
      // Add throwIfReplStopped if this is a backward branch (jumping to a label we've already seen)
      if seenLabels.contains(label) then addStopCheck()
      super.visitJumpInsn(opcode, label)

end ReplBytecodeInstrumentation
