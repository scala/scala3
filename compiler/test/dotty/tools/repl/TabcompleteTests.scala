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

  @Test def tabCompleteList: Unit = fromInitialState { implicit s =>
    val comp = tabComplete("List.r")
    assertTrue(s"""Expected single element "range" got: ${comp.suggestions}""",
      comp.suggestions.head == "range")
  }

  @Test def tabCompleteListInstance: Unit = fromInitialState { implicit s =>
    val comp = tabComplete("(null: List[Int]).sli")
    assertTrue(s"""Expected completions "slice" and "sliding": ${comp.suggestions}""",
      comp.suggestions.sorted == List("slice", "sliding"))
  }

  @Test def autoCompleteValAssign: Unit =
    fromInitialState { implicit s => tabComplete("val x = 5") }

  @Test def tabCompleteNumberDot: Unit =
    fromInitialState { implicit s => tabComplete("val x = 5 + 5.") }

  @Test def tabCompleteInClass: Unit =
    fromInitialState { implicit s =>
      tabComplete("class Foo { def bar: List[Int] = List.a")
    }

  @Test def tabCompleteTwiceIn: Unit = {
    val src1 = "class Foo { def bar: List[Int] = List.a"
    val src2 = "class Foo { def bar: List[Int] = List.app"

    fromInitialState { implicit state =>
      assert(tabComplete(src1).suggestions.nonEmpty)
      state
    }
    .andThen { implicit state =>
      assert(tabComplete(src2).suggestions.nonEmpty)
    }
  }

  @Test def i3309: Unit =
    fromInitialState { implicit s =>
      List("\"", "#", ")", "=", "'", "¨", "£", ".", ":", ",", ";", "@", "}", "[", "]")
        .foreach(src => assertTrue(tabComplete(src).suggestions.isEmpty))
    }
}
