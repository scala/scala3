package dotty.tools.dotc

import org.junit.Assert._
import org.junit.Test
import dotty.tools.backend.jvm._
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.dotc.core.Contexts.FreshContext
import scala.tools.asm.tree.MethodNode

class SimplifyPosTests extends DottyBytecodeOptimisedTest with SimplifyEquivalences
class SimplifyNegTests extends DottyBytecodeTest          with SimplifyEquivalences

class DottyBytecodeOptimisedTest extends DottyBytecodeTest {
  override protected def initializeCtx(c: FreshContext): Unit = {
    super.initializeCtx(c)
    val flags = Array("-optimise") // :+ "-Xprint:simplify"
    val summary = CompilerCommand.distill(flags)(c)
    c.setSettings(summary.sstate)
  }
}

trait SimplifyEquivalences { self: DottyBytecodeTest =>
  def check(expr1: String, expr2: String, shared: String = ""): Unit = {
    import ASMConverters._
    val source =
      s"""
      $shared
      |class A {
      |  def main(): Unit = {
            $expr1
      |  }
      |}
      |class B {
      |  def main(): Unit = {
            $expr2
      |  }
      |}
      """.stripMargin
    checkBCode(source) { dir =>
      def instructions(clazz: String): List[Instruction] = {
        val clsIn   = dir.lookupName(s"$clazz.class", directory = false).input
        val clsNode = loadClassNode(clsIn)
        instructionsFromMethod(getMethod(clsNode, "main"))
      }
      val A = instructions("A")
      val B = instructions("B")
      val diff = diffInstructions(A, B)
      if (this.isInstanceOf[DottyBytecodeOptimisedTest])
        assert(A == B, s"Bytecode wasn't same:\n$diff")
      else
        assert(A != B, s"Bytecode was the same:\n$diff")
    }
  }

  @Test def inlineVals =
    check("println(1)",
       """
          |val one = 1
          |val anotherone = one
          |println(anotherone)
       """)

  @Test def inlineCaseIntrinsicsDottyApply =
    check(
      expr1  = "CC.apply(1, 2)",
      expr2  = "new CC(1, 2)",
      shared = "case class CC(i: Int, j: Int)")

  @Test def inlineCaseIntrinsicsScalacApply =
    check("::.apply(1, Nil)", "new ::(1, Nil)")

  @Test def inlineCaseIntrinsicsScalacUnapply =
    check(
      """
         |val t = Tuple2(1, "s")
         |print(Tuple2.unapply(t))
      """,
      """
         |val t = Tuple2(1, "s")
         |print(new Some(new Tuple2(t._1, t._2)))
      """)

  @Test def constantFold =
    check(
      """
         |val t = true // val needed, or typer takes care of this
         |if (t) print(1)
         |else   print(2)
      """,
      """
         |print(1)
      """)

  @Test def dropNoEffects =
    check(
      """
         |"wow"
         |print(1)
      """,
      """
         |print(1)
      """)

  // @Test def inlineOptions =
  //   check(
  //     """
  //        |val sum = Some("s")
  //        |println(sum.isDefined)
  //     """,
  //     """
  //        |println(true)
  //     """)
}
