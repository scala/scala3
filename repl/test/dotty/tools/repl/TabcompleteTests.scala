package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

import dotc.reporting.MessageRendering
import dotc.ast.untpd

import results._
import ReplTest._

/** These tests test input that has proved problematic */
class TabcompleteTests extends ReplTest {

  private[this] def parseString(str: String) = {
    val res = ParseResult(str)(myCtx)

    if (!res.isInstanceOf[Parsed])
      fail(s"""Expected `Parsed("$str", ...)` got: $res""")

    res.asInstanceOf[Parsed]
  }

  private[this] def parseWithErrors(str: String) = {
    val res = ParseResult(str)(myCtx)

    if (!res.isInstanceOf[SyntaxErrors])
      fail(s"""Expected `SyntaxErrors` got: $res""")

    Parsed(str, res.asInstanceOf[SyntaxErrors].trees)
  }

  /** Returns the `(<instance completions>, <companion completions>)`*/
  private[this] def tabComplete(parsed: Parsed): Completions = {
    val src = parsed.sourceCode
    completions(src.length, src, initState)
  }

  private[this] def tabComplete(str: String): Completions =
    (parseString _ andThen tabComplete)(str)

  private[this] def tabCompleteWithErrors(str: String): Completions =
    (parseWithErrors _ andThen tabComplete)(str)

  @Test def tabCompleteList: Unit = {
    val comp = tabComplete("List.r")
    assertTrue(s"expected empty instance completion, got: ${comp.instance}",
      comp.instance.isEmpty)
    assertTrue(s"""Expected single element "range" got: ${comp.companion}""",
      comp.companion.head == "range")
  }

  @Test def tabCompleteListInstance: Unit = {
    val comp = tabComplete("(null: List[Int]).sli")
    assertTrue(s"expected empty instance completion, got: ${comp.companion}",
      comp.companion.isEmpty)
    assertTrue(s"""Expected completions "slice" and "sliding": ${comp.instance}""",
      comp.instance.sorted == List("slice", "sliding"))
  }

  @Test def autoCompletValAssign: Unit =
    tabComplete("val x = 5")

  @Test def tabCompleteNumberDot: Unit =
    tabCompleteWithErrors("val x = 5 + 5.")
}
