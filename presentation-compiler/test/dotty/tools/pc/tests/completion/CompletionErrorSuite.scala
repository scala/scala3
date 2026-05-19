package dotty.tools.pc.tests.completion

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Ignore
import org.junit.Test

class CompletionErrorSuite extends BaseCompletionSuite {

  @Test def method =
    check(
      s"""|case class Person(name: String, age: Int)
          |
          |object Demo {
          |
          |  def hello(name: String, age: Int): Person = ???
          |
          |  def demo() = {
          |    val z = hello("a", "b").@@
          |  }
          |
          |}
          |""".stripMargin,
      """|age: Int
         |name: String
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def method2 =
    check(
      s"""|case class Person(name: String, age: Int)
          |
          |object Demo {
          |
          |  def hello(name: String, age: Int): Person = ???
          |
          |  def demo() = {
          |    val z = hello("a", "b").a@@
          |  }
          |
          |}
          |""".stripMargin,
      """|age: Int
         |asInstanceOf[X0]: X0
         |""".stripMargin,
      topLines = Option(2)
    )
}
