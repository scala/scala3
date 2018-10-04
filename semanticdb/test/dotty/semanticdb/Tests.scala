package dotty.semanticdb

import scala.tasty.Tasty
import scala.tasty.util.TreeTraverser
import scala.tasty.file._

import org.junit.Test
import org.junit.Assert._

class Tests {

  // TODO: update scala-0.10 on version change (or resolve automatically)
  final def testClasspath = "out/bootstrap/dotty-semanticdb/scala-0.10/test-classes"

  @Test def testMain(): Unit = {
    testOutput(
      "tests.SimpleClass",
      "SimpleClass;<init>;"
    )
    testOutput(
      "tests.SimpleDef",
      "SimpleDef;<init>;foo;"
    )
  }

  def testOutput(className: String, expected: String): Unit = {
    val out = new StringBuilder
    ConsumeTasty(testClasspath, List(className), new DBConsumer {
      override def println(x: Any): Unit = out.append(x).append(";")
    })
    assertEquals(expected, out.result())
  }
}
