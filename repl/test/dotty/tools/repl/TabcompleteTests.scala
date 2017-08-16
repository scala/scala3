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

  /** Returns the `(<instance completions>, <companion completions>)`*/
  private[this] def tabComplete(src: String)(implicit state: State): Completions =
    completions(src.length, src, state)

  @Test def tabCompleteList: Unit = withState {
    val comp = tabComplete("List.r")
    assertTrue(s"""Expected single element "range" got: ${comp.suggestions}""",
      comp.suggestions.head == "range")
  }

  @Test def tabCompleteListInstance: Unit = withState {
    val comp = tabComplete("(null: List[Int]).sli")
    assertTrue(s"""Expected completions "slice" and "sliding": ${comp.suggestions}""",
      comp.suggestions.sorted == List("slice", "sliding"))
  }

  @Test def autoCompletValAssign =
    withState[Unit](tabComplete("val x = 5"))

  @Test def tabCompleteNumberDot =
    withState[Unit](tabComplete("val x = 5 + 5."))

  @Test def tabCompleteInClass =
    withState[Unit](tabComplete("class Foo { def bar: List[Int] = List.a"))

  @Test def tabCompleteTwiceIn = {
    val src1 = "class Foo { def bar: List[Int] = List.a"
    val src2 = "class Foo { def bar: List[Int] = List.app"

    withState {
      assert(tabComplete(src1).suggestions.nonEmpty)
      fromState(implicitly[State]) {
        assert(tabComplete(src2).suggestions.nonEmpty)
      }
    }
  }
}
