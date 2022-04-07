package dotty
package tools
package backend.jvm

import scala.language.unsafeNulls

import vulpix.TestConfiguration

import dotc.core.Contexts.{Context, ContextBase, ctx}
import dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotc.core.Phases.Phase
import dotc.Compiler

import dotty.tools.io.{VirtualDirectory => Directory}
import scala.tools.asm
import asm._
import asm.tree._
import scala.collection.JavaConverters._

import io.{AbstractFile, JavaClassPath, VirtualDirectory}
import scala.collection.JavaConverters._
import scala.tools.asm.{ClassWriter, ClassReader}
import scala.tools.asm.tree._
import java.io.{File => JFile, InputStream}

import org.junit.Assert._

trait DottyBytecodeTest {
  import AsmNode._
  import ASMConverters._
  import DottyBytecodeTest._

  protected object Opcode {
    val newarray       = 188
    val anewarray      = 189
    val multianewarray = 197

    val boolean        = 4
    val char           = 5
    val float          = 6
    val double         = 7
    val byte           = 8
    val short          = 9
    val int            = 10
    val long           = 11

    val boxedUnit      = "scala/runtime/BoxedUnit"
    val javaString     = "java/lang/String"
  }

  def initCtx = {
    val ctx0 = (new ContextBase).initialCtx.fresh
    val outputDir = new VirtualDirectory("<DottyBytecodeTest output>")
    ctx0.setSetting(ctx0.settings.silentWarnings, true)
    ctx0.setSetting(ctx0.settings.classpath, TestConfiguration.basicClasspath)
    ctx0.setProperty(ContextDoc, new ContextDocstrings)
    ctx0.setSetting(ctx0.settings.outputDir, outputDir)
  }

  def checkBCode(scalaSource: String)(checkOutput: AbstractFile => Unit): Unit =
    checkBCode(List(scalaSource))(checkOutput)

  /** Checks source code from raw strings */
  def checkBCode(scalaSources: List[String], javaSources: List[String] = Nil)(checkOutput: AbstractFile => Unit): Unit = {
    given Context = initCtx

    val compiler = new Compiler
    val run = compiler.newRun
    compiler.newRun.compileFromStrings(scalaSources, javaSources)

    checkOutput(ctx.settings.outputDir.value)
  }

  def compileCode(scalaSources: List[String], javaSources: List[String] = Nil): AbstractFile = {
    given Context = initCtx

    val compiler = new Compiler
    val run = compiler.newRun
    compiler.newRun.compileFromStrings(scalaSources, javaSources)
    ctx.settings.outputDir.value
  }

  def getGeneratedClassfiles(outDir: AbstractFile): List[(String, Array[Byte])] = {
    import scala.collection.mutable.ListBuffer
    def files(dir: AbstractFile): List[(String, Array[Byte])] = {
      val res = ListBuffer.empty[(String, Array[Byte])]
      for (f <- dir.iterator) {
        if (!f.isDirectory) res += ((f.name, f.toByteArray))
        else if (f.name != "." && f.name != "..") res ++= files(f)
      }
      res.toList
    }
    files(outDir)
  }

  protected def loadClassNode(input: InputStream, skipDebugInfo: Boolean = true): ClassNode = {
    val cr = new ClassReader(input)
    val cn = new ClassNode()
    cr.accept(cn, if (skipDebugInfo) ClassReader.SKIP_DEBUG else 0)
    cn
  }

  /** Finds a class with `cls` as name in `dir`, throws if it can't find it */
  def findClass(cls: String, dir: AbstractFile) = {
    val clsIn = dir.lookupName(s"$cls.class", directory = false).input
    val clsNode = loadClassNode(clsIn)
    assert(clsNode.name == cls, s"inspecting wrong class: ${clsNode.name}")
    clsNode
  }

  protected def getMethod(classNode: ClassNode, name: String): MethodNode =
    classNode.methods.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find method '$name' in class '${classNode.name}'")

  protected def getField(classNode: ClassNode, name: String): FieldNode =
    classNode.fields.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find field '$name' in class '${classNode.name}'")

  def getInstructions(c: ClassNode, name: String): List[Instruction] =
    instructionsFromMethod(getMethod(c, name))

  def assertSameCode(method: MethodNode, expected: List[Instruction]): Unit =
    assertSameCode(instructionsFromMethod(method).dropNonOp, expected)
  def assertSameCode(actual: List[Instruction], expected: List[Instruction]): Unit = {
    assert(actual === expected, "\n" + diffInstructions(actual, expected))
  }

  def assertInvoke(m: MethodNode, receiver: String, method: String): Unit =
    assertInvoke(instructionsFromMethod(m), receiver, method)
  def assertInvoke(l: List[Instruction], receiver: String, method: String): Unit = {
    assert(l.exists {
      case Invoke(_, `receiver`, `method`, _, _) => true
      case _ => false
    }, l.stringLines)
  }

  def diffInstructions(isa: List[Instruction], isb: List[Instruction]): String = {
    val len = Math.max(isa.length, isb.length)
    val sb = new StringBuilder
    if (len > 0 ) {
      val width = isa.map(_.toString.length).max
      val lineWidth = len.toString.length
      (1 to len) foreach { line =>
        val isaPadded = isa.map(_.toString) orElse LazyList.continually("")
        val isbPadded = isb.map(_.toString) orElse LazyList.continually("")
        val a = isaPadded(line-1)
        val b = isbPadded(line-1)

        sb append (s"""$line${" " * (lineWidth-line.toString.length)} ${if (a==b) "==" else "<>"} $a${" " * (width-a.length)} | $b\n""")
      }
    }
    sb.toString
  }

  /**************************** Comparison Methods ****************************/
  def verifySwitch(method: MethodNode, shouldFail: Boolean = false, debug: Boolean = false): Boolean = {
    val instructions = instructionsFromMethod(method)

    val succ = instructions
      .collect {
        case x: TableSwitch  => x
        case x: LookupSwitch => x
      }
      .length > 0

    if (debug || !succ && !shouldFail || succ && shouldFail)
      instructions.foreach(Console.err.println)

    succ && !shouldFail || shouldFail && !succ
  }

  def sameBytecode(methA: MethodNode, methB: MethodNode) = {
    val isa = instructionsFromMethod(methA)
    val isb = instructionsFromMethod(methB)
    assert(isa == isb, s"Bytecode wasn't same:\n${diffInstructions(isa, isb)}")
  }

  def similarBytecode(
    methA:   MethodNode,
    methB:   MethodNode,
    similar: (List[Instruction], List[Instruction]) => Boolean
  ) = {
    val isa = instructionsFromMethod(methA)
    val isb = instructionsFromMethod(methB)
    assert(
      similar(isa, isb),
      s"""|Bytecode wasn't similar according to the provided predicate:
          |${diffInstructions(isa, isb)}""".stripMargin)
  }

  def sameMethodAndFieldSignatures(clazzA: ClassNode, clazzB: ClassNode) =
    sameCharacteristics(clazzA, clazzB)(_.characteristics)

  /**
   * Same as sameMethodAndFieldSignatures, but ignoring generic signatures.
   * This allows for methods which receive the same descriptor but differing
   * generic signatures. In particular, this happens with value classes, which
   * get a generic signature where a method written in terms of the underlying
   * values does not.
   */
  def sameMethodAndFieldDescriptors(clazzA: ClassNode, clazzB: ClassNode): Unit = {
    val (succ, msg) = sameCharacteristics(clazzA, clazzB)(_.erasedCharacteristics)
    assert(succ, msg)
  }

  private def sameCharacteristics(clazzA: ClassNode, clazzB: ClassNode)(f: AsmNode[_] => String): (Boolean, String) = {
    val ms1 = clazzA.fieldsAndMethods.toIndexedSeq
    val ms2 = clazzB.fieldsAndMethods.toIndexedSeq
    val name1 = clazzA.name
    val name2 = clazzB.name

    if (ms1.length != ms2.length) {
      (false, s"Different member counts in $name1 and $name2")
    } else {
      val msg     = new StringBuilder
      val success = ms1.lazyZip(ms2) forall { (m1, m2) =>
        val c1 = f(m1)
        val c2 = f(m2).replace(name2, name1)
        if (c1 == c2)
          msg append (s"[ok] $m1")
        else
          msg append (s"[fail]\n  in $name1: $c1\n  in $name2: $c2")

        c1 == c2
      }

      (success, msg.toString)
    }
  }

  def correctNumberOfNullChecks(expectedChecks: Int, insnList: InsnList) = {
    /** Is given instruction a null check?
     *
     *  This will detect direct null comparison as in
     *    if (x == null) ...
     *  and not indirect as in
     *    val foo = null
     *    if (x == foo) ...
     */
    def isNullCheck(node: asm.tree.AbstractInsnNode): Boolean = {
      val opcode = node.getOpcode
      (opcode == asm.Opcodes.IFNULL) || (opcode == asm.Opcodes.IFNONNULL)
    }
    val actualChecks = insnList.iterator.asScala.count(isNullCheck)
    assert(expectedChecks == actualChecks,
      s"Wrong number of null checks ($actualChecks), expected: $expectedChecks"
    )
  }

  def assertBoxing(nodeName: String, methods: java.lang.Iterable[MethodNode])(implicit source: String): Unit =
    methods.asScala.find(_.name == nodeName)
    .map { node =>
      val (ins, boxed) = boxingInstructions(node)
      if (!boxed) fail("No boxing in:\n" + boxingError(ins, source))
    }
    .getOrElse(fail("Could not find constructor for object `Test`"))

  private def boxingError(ins: List[_], source: String) =
    s"""|----------------------------------
        |${ins.mkString("\n")}
        |----------------------------------
        |From code:
        |$source
        |----------------------------------""".stripMargin


  protected def assertNoBoxing(nodeName: String, methods: java.lang.Iterable[MethodNode])(implicit source: String): Unit =
    methods.asScala.find(_.name == nodeName)
    .map { node =>
      val (ins, boxed) = boxingInstructions(node)
      if (boxed) fail(boxingError(ins, source))
    }
    .getOrElse(fail("Could not find constructor for object `Test`"))

  protected def boxingInstructions(method: MethodNode): (List[_], Boolean) = {
    val ins = instructionsFromMethod(method)
    val boxed = ins.exists {
      case Invoke(op, owner, name, desc, itf) =>
        owner.toLowerCase.contains("box") || name.toLowerCase.contains("box")
      case _ => false
    }

    (ins, boxed)
  }

  protected def hasInvokeStatic(method: MethodNode): Boolean = {
    val ins = instructionsFromMethod(method)
    ins.exists {
      case Invoke(op, owner, name, desc, itf) =>
        op == 184
      case _ => false
    }
  }

}
object DottyBytecodeTest {
  extension [T](l: List[T]) def stringLines = l.mkString("\n")
}
