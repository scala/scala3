package test

import org.junit.Assert._
import org.junit.Test

class TestBCode extends DottyBytecodeTest {
  import ASMConverters._
  @Test def nullChecks = {
    val source = """
                 |class Foo {
                 |  def foo(x: AnyRef): Int = {
                 |    val bool = x == null
                 |    if (x != null) 1
                 |    else 0
                 |  }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode = getMethod(clsNode, "foo")
      correctNumberOfNullChecks(2, methodNode.instructions)
    }
  }

  /** This test verifies that simple matches are transformed if possible
   *  despite no annotation
   */
  @Test def basicTransformNonAnnotated = {
    val source = """
                 |object Foo {
                 |  def foo(i: Int) = i match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  /** This test verifies that simple matches with `@switch` annotations are
   *  indeed transformed to a switch
   */
  @Test def basicTransfromAnnotated = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Int) = (i: @switch) match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def failTransform = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Any) = (i: @switch) match {
                 |    case x: String => println("string!")
                 |    case x :: xs   => println("list!")
                 |  }
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")

      assert(verifySwitch(methodNode, shouldFail = true))
    }
  }

  /** Make sure that creating multidim arrays reduces to "multinewarray"
   *  instruction
   */
  @Test def multidimArrays = {
    val source = """
                 |object Arr {
                 |  def arr = Array.ofDim[Int](2, 1)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method     = getMethod(moduleNode, "arr")

      val hadCorrectInstr =
        instructionsFromMethod(method)
        .collect { case x @ NewArray(op, _, dims) if op == 197 && dims == 2 => x }
        .length > 0

      assert(hadCorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(method).mkString("\n"))
    }
  }
}
