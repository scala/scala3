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

  /** Returns the `(<instance completions>, <companion completions>)`*/
  private[this] def autoComplete(parsed: Parsed): Completions = {
    val src = parsed.sourceCode
    completions(src.length, src, initState)
  }

  private[this] def autoComplete(str: String): Completions =
    (parseString _ andThen autoComplete)(str)

  @Test def autoCompleteList: Unit = {
    val comp = autoComplete("List.r")
    assertTrue(s"expected empty instance completion, got: ${comp.instance}",
      comp.instance.isEmpty)
    assertTrue(s"""Expected single element "range" got: ${comp.companion}""",
      comp.companion.head == "range")
  }

  @Test def autoCompleteListInstance: Unit = {
    val comp = autoComplete("(null: List[Int]).sli")
    assertTrue(s"expected empty instance completion, got: ${comp.instance}",
      comp.companion.isEmpty)
    assertTrue(s"""Expected single element "range" got: ${comp.companion}""",
      comp.instance == List("slice", "sliding"))
  }

  @Test def autoCompletValAssign: Unit =
    autoComplete("val x = 5")
}
