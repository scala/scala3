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

  @Test def tabCompleteModule: Unit =
    fromInitialState{ implicit s =>
      val comp = tabComplete("scala.Pred")
      assertEquals(comp.suggestions,List("Predef"))
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
      // TODO: add back '.', once #4397 is fixed
      List("\"", ")", "'", "¨", "£", ":", ",", ";", "@", "}", "[", "]")
        .foreach(src => assertTrue(tabComplete(src).suggestions.isEmpty))
    }

  @Test def sortedCompletions: Unit =
    fromInitialState { implicit state  =>
      val src = "class Foo { def comp3 = 3; def comp1 = 1; def comp2 = 2 }"
      compiler.compile(src).stateOrFail
    }
    .andThen { implicit state =>
      val expected = List("comp1", "comp2", "comp3")
      assertEquals(expected, tabComplete("(new Foo).comp").suggestions)
    }
}
