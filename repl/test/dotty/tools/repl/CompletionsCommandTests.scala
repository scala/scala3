package dotty.tools
package repl

import org.junit.Assert.*
import org.junit.Test

/** Tests for the `:completions <input>` command used by editors to obtain
 *  completion candidates through a dumb terminal.
 *
 *  Ported from the Scala 2 test `test/files/run/repl-completions.scala`.
 */
class CompletionsCommandTests extends ReplTest:

  private def completionLines(): List[String] =
    storedOutput().linesIterator.toList

  private val defineO = "object O { def x_y_x = 1; def x_y_z = 2; def getFooBarZot = 3 }"

  @Test def completesMemberSelection = initially {
    run(":completions List(1).ma")
    val lines = completionLines()
    assertTrue(lines.toString, lines.contains("[completions] List(1).map"))
    assertTrue(lines.toString, lines.contains("[completions] List(1).max"))
    assertTrue(lines.toString, lines.forall(_.startsWith("[completions] List(1).ma")))
    assertEquals(lines.distinct, lines)
  }

  @Test def completesMembersOfUserObject =
    initially {
      run(defineO)
    } andThen {
      storedOutput() // discard output
      run(":completions O.x")
      assertEquals(List("[completions] O.x_y_x", "[completions] O.x_y_z"), completionLines())
    }

  @Test def exactMatch =
    initially {
      run(defineO)
    } andThen {
      storedOutput() // discard output
      run(":completions O.x_y_x")
      assertEquals(List("[completions] O.x_y_x"), completionLines())
    }

  @Test def printsNothingWithoutCandidates =
    initially {
      run(defineO)
    } andThen {
      storedOutput() // discard output
      run(":completions O.x_y_a")
      assertEquals("", storedOutput())
      run(":completions List(1).thisDoesNotExist")
      assertEquals("", storedOutput())
    }

  @Test def completesImportedMembers =
    initially {
      run(defineO)
    } andThen {
      run("import O.*")
    } andThen {
      storedOutput() // discard output
      run(":completions x_y_")
      assertEquals(List("[completions] x_y_x", "[completions] x_y_z"), completionLines())
      run(":completions x_y_a")
      assertEquals("", storedOutput())
    }

  @Test def completesInsideStatement =
    initially {
      run(defineO)
    } andThen {
      storedOutput() // discard output
      run(":completions object O2 { val x = O.")
      val lines = completionLines()
      val expected = List("getFooBarZot", "x_y_x", "x_y_z").map(m => s"[completions] object O2 { val x = O.$m")
      assertEquals(lines.toString, expected, lines.filter(l => expected.contains(l)))
      assertTrue(lines.toString, lines.forall(_.startsWith("[completions] object O2 { val x = O.")))
    }

  @Test def completesCommandNames = initially {
    run(":completions :he")
    assertEquals(List("[completions] :help"), completionLines())
    run(":completions :completion")
    assertEquals(List("[completions] :completions"), completionLines())
  }

  @Test def doesNotChangeState = initially {
    val state = run(":completions List(1).ma")
    storedOutput() // discard output
    assertEquals(0, state.objectIndex)
    run("val x = 1")(using state)
    assertEquals("val x: Int = 1", storedOutput().trim)
  }

  @Test def listedInHelp = initially {
    run(":help")
    assertTrue(storedOutput().contains(":completions <input>"))
  }
