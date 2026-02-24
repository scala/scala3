package dotty.tools.pc.tests.completion

import scala.meta.pc.CompletionItemPriority

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionContextSuite extends BaseCompletionSuite:
  override val completionItemPriority: CompletionItemPriority = {
    case "scala/concurrent/Future." => -1
    case _ => 0
  }
  // scala.concurrent.Future should be ranked higher than java.util.concurrent.Future
  val futureCompletionResult: List[String] =
    List("Future - scala.concurrent", "Future - java.util.concurrent")

  @Test
  def `context` =
    check(
      """package fut
        |object A {
        |  Futur@@
        |}""".stripMargin,
      """Future - scala.concurrent
        |Future - java.util.concurrent
        |""".stripMargin,
      filter = futureCompletionResult.contains
    )

  @Test def multipleFoos =
    checkEdit(
      """|
         |package a{
         |  class Foo
         |}
         |package b{
         |  class Foo
         |}
         |package c{
         |  class Foo
         |}
         |package d{
         |  import a.{Foo => Bar}
         |  import b.Foo
         |
         |  val x = new Foo@@ // I want to import c.Foo
         |}""".stripMargin,
      """|
         |package a{
         |  class Foo
         |}
         |package b{
         |  class Foo
         |}
         |package c{
         |  class Foo
         |}
         |package d{
         |  import a.{Foo => Bar}
         |  import b.Foo
         |
         |  val x = new c.Foo // I want to import c.Foo
         |}""".stripMargin,
      assertSingleItem = false,
      filter = (label: String) => label.contains("Foo - c")
    )
