package test

import org.junit.Assert._
import org.junit.Test

class TestBCode extends DottyBytecodeTest {
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

    checkBCode(source) { file =>
      val clsIn      = file.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode = getMethod(clsNode, "foo")
      correctNumberOfNullChecks(2, methodNode.instructions)
    }
  }
}
