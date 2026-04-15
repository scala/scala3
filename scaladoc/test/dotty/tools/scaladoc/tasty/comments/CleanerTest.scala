package dotty.tools.scaladoc.tasty.comments

import org.junit.Test
import org.junit.Assert.assertEquals
import dotty.tools.scaladoc.tasty.comments.Cleaner

class CleanerTest {
  @Test def `remove only one space in comments`: Unit = {
    val example = """
      |/** ```
      | * def x =
      | *   2
      | * ```
      | */
    """.stripMargin

    val result = Cleaner.clean(example)

    assertEquals(List(
      "```",
      "def x =",
      "  2",
      "```",
      ""
    ), result)
  }

  @Test def `remove trailing whitespaces`: Unit = {
    // example with (invisible) whitespaces at the end of line
    val example = """
      |/**
      | * no whitespace:
      | * one whitespace: 
      | * two whitespaces:  
      | */
    """.stripMargin

    val result = Cleaner.clean(example)

    assertEquals(List(
      "",
      "no whitespace:",
      "one whitespace:",
      "two whitespaces:",
      ""
    ), result)
  }
}
