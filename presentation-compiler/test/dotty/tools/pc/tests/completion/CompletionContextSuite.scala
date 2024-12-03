package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite
import scala.meta.pc.CompletionItemPriority
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
