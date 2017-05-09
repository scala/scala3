package dotty.tools
package backend.jvm

import dotc.core.Contexts.{Context, ContextBase}
import dotc.core.Phases.Phase
import dotc.Compiler

import dotty.tools.io.{VirtualDirectory => Directory}
import scala.tools.asm
import asm._
import asm.tree._
import scala.collection.JavaConverters._

import io.JavaClassPath
import scala.collection.JavaConverters._
import scala.tools.asm.{ClassWriter, ClassReader}
import scala.tools.asm.tree._
import java.io.{File => JFile, InputStream}

class TestGenBCode(val outDir: String) extends GenBCode {
  override def phaseName: String = "testGenBCode"
  val virtualDir = new Directory(outDir, None)
  override def outputDir(implicit ctx: Context) = virtualDir
}

trait DottyBytecodeTest extends DottyTest {
  import AsmNode._
  import ASMConverters._

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

  private def bCodeCheckingComp(phase: TestGenBCode)(check: Directory => Unit) =
    new Compiler {
      override def phases = {
        val updatedPhases = {
          def replacePhase: Phase => Phase =
            { p => if (p.phaseName == "genBCode") phase else p }

          for (phaseList <- super.phases) yield phaseList.map(replacePhase)
        }

        val checkerPhase = List(List(new Phase {
          def phaseName = "assertionChecker"
          override def run(implicit ctx: Context): Unit =
            check(phase.virtualDir)
        }))

        updatedPhases ::: checkerPhase
      }
    }

  private def outPath(obj: Any) =
      "/genBCodeTest" + math.abs(obj.hashCode) + System.currentTimeMillis

  /** Checks source code from raw string */
  def checkBCode(source: String)(assertion: Directory => Unit) = {
    val comp = bCodeCheckingComp(new TestGenBCode(outPath(source)))(assertion)
    comp.rootContext(ctx)
    comp.newRun.compile(source)
  }

  /** Checks actual _files_ referenced in `sources` list */
  def checkBCode(sources: List[String])(assertion: Directory => Unit) = {
    val comp = bCodeCheckingComp(new TestGenBCode(outPath(sources)))(assertion)
    comp.rootContext(ctx)
    comp.newRun.compile(sources)
  }

  protected def loadClassNode(input: InputStream, skipDebugInfo: Boolean = true): ClassNode = {
    val cr = new ClassReader(input)
    val cn = new ClassNode()
    cr.accept(cn, if (skipDebugInfo) ClassReader.SKIP_DEBUG else 0)
    cn
  }

  protected def getMethod(classNode: ClassNode, name: String): MethodNode =
    classNode.methods.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find method '$name' in class '${classNode.name}'")

  def diffInstructions(isa: List[Instruction], isb: List[Instruction]): String = {
    val len = Math.max(isa.length, isb.length)
    val sb = new StringBuilder
    if (len > 0 ) {
      val width = isa.map(_.toString.length).max
      val lineWidth = len.toString.length
      (1 to len) foreach { line =>
        val isaPadded = isa.map(_.toString) orElse Stream.continually("")
        val isbPadded = isb.map(_.toString) orElse Stream.continually("")
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
      val success = (ms1, ms2).zipped forall { (m1, m2) =>
        val c1 = f(m1)
        val c2 = f(m2).replaceAllLiterally(name2, name1)
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
}
