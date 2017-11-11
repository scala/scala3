package dotty.tools.dotc

import org.junit.Assert._
import org.junit.Test
import dotty.tools.backend.jvm._
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.dotc.core.Contexts.FreshContext
import scala.tools.asm.tree.MethodNode

class LocalOptPosTests extends LocalOptTests(optimise = true)
class LocalOptNegTests extends LocalOptTests(optimise = false)

abstract class LocalOptTests(val optimise: Boolean) extends DottyBytecodeTest {
  override protected def initializeCtx(c: FreshContext): Unit = {
    super.initializeCtx(c)
    if (optimise) {
      val flags = Array("-optimise") // :+ "-Xprint:simplify"
      val summary = CompilerCommand.distill(flags)(c)
      c.setSettings(summary.sstate)
    }
  }


  def checkneq(source: String, expected: String, shared: String = "") = check(source, expected, shared, false)

  def check(source: String, expected: String, shared: String = "", equal: Boolean = true): Unit = {
    import ASMConverters._
    val src =
      s"""
      $shared
      |class A {
      |  def main(): Unit = {
            $source
      |  }
      |}
      |class B {
      |  def main(): Unit = {
            $expected
      |  }
      |}
      """.stripMargin
    checkBCode(src) { dir =>
      def instructions(clazz: String): List[Instruction] = {
        val clsIn   = dir.lookupName(s"$clazz.class", directory = false).input
        val clsNode = loadClassNode(clsIn)
        instructionsFromMethod(getMethod(clsNode, "main"))
      }
      val A = instructions("A")
      val B = instructions("B")
      val diff = diffInstructions(A, B)
      if(equal) {
        if (optimise)
          assert(A == B, s"Bytecode doesn't match: (lhs = source, rhs = expected) \n$diff")
        else
          assert(A != B, s"Same Bytecodes without -optimise: you are testing the wrong thing!")
      } else {
        assert(A != B, s"Bytecode match (they shouldn't): (lhs = source, rhs = expected) \n$diff")
      }
    }
  }


  /* 
   * Null check removal tests
   */

  @Test def RedundantNullChecks =
    check(
      """
        |val i = readLine()
        |if (i != null) {
        |  if (null == i) () else print(i)
        |}
        |i.length
        |if (i != null) print(i)
      """,
      """
        |val i = readLine()
        |if (i != null) print(i)
        |i.length
        |print(i)
      """)

  @Test def MultipleNullChecks =
    check(
      """
        |val i = readLine()
        |val j = readLine()
        |val k = readLine()
        |if (i != null && j != null && k == null) {
        |  if(i == null) () else print(i)
        |  if(j != null) print(j)
        |  if(k != null) print(k)
        |}
      """,
      """
        |val i = readLine()
        |val j = readLine()
        |val k = readLine()
        |if (i != null && j != null && k == null) {
        |  print(i)
        |  print(j)
        |}
      """)

  // (this != null) won't be optimized but I don't expect it to ever happen
  @Test def ThisNullChecks =
    check(
      """
        |val self = this
        |if(self != null) {
        |  print(13)
        |} else {
        |  print(9)
        |}
      """,
      """
        |val self = this
        |print(13)
      """)

  @Test def NewNullChecks =
    check(
      """
        |val i = new String("a string")
        |if(i != null) {
        |  print(7)
        |} else {
        |  print(9)
        |}
      """,
      """
        |val i = new String("a string")
        |print(7)
      """)    


  /*
   * Constant folding tests
   */

  @Test def BasicConstantFold =
    check(
      """
        |val i = 3
        |val j = i + 4
        |print(j)
      """,
      """
        |print(7)
      """)

  @Test def ConstantFold =
    check(
      """
        |val i = 3
        |val j = i + 4
        |if(j - i >= (i + 1) / 2) 
        |  print(i + 1)
      """,
      """
        |print(4)
      """)

  @Test def TwoValConstantFold =
    check(
      """
        |val i = 3
        |val j = 4
        |val k = i * j
        |print(k - j)
      """,
      """
        |print(8)
      """)


  // check for incorrecly eliminated statements with side effects 
  /*@Test def ArrayLoadInitValify =
    checkneq(
      """
        |val ar = Array.ofDim[Int](5)
      """, """""")

  @Test def ArrayLoadValify =
    checkneq(
      """
        |val ar = Array.ofDim[Int](5)
        |var x = 0
        |while (x<=5) {
        |  println(x)
        |  val a = ar(x)
        |  x+=1
        |}
      """,
      """
        |val ar = Array.ofDim[Int](5)
        |var x = 0
        |while (x<=5) {
        |  println(x)
        |  x+=1
        |}
      """)*/

}
