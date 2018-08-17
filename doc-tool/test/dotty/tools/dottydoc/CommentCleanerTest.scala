package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import model.comment.CommentCleaner

class CommentCleanerTest extends CommentCleaner {
  @Test def simpleOneliner = {
    assertEquals(List("lol"), clean("/** lol */"))
  }

  @Test def multiline = {
    val docstring = clean {
      """|/** First
         | *  Second
         | */
         |""".stripMargin
    }

    assertEquals(List("First", "Second", ""), docstring)
  }

  @Test def multilineBad = {
    val docstring = clean {
      """|/** First
         | *   Second
         | */
         |""".stripMargin
    }

    assertEquals(List("First", " Second", ""), docstring)
  }

  @Test def multilineWorse = {
    val docstring = clean {
      """|/** First
         | *   Second
         | *  Third
         | */
         |""".stripMargin
    }

    assertEquals(List("First", " Second", "Third", ""), docstring)
  }

  @Test def multilineFirstNoSpace = {
    val docstring = clean {
      """|/**First
         | *   Second
         | *  Third
         | */
         |""".stripMargin
    }

    assertEquals(List("First", " Second", "Third", ""), docstring)
  }

  @Test def multilineFirstTwoSpaces = {
    val docstring = clean {
      """|/**  First
         | *   Second
         | *  Third
         | */
         |""".stripMargin
    }

    assertEquals(List("First", " Second", "Third", ""), docstring)
  }

  @Test def multilineFirstThreeSpaces = {
    val docstring = clean {
      """|/**   First
         | *   Second
         | *  Third
         | */
         |""".stripMargin
    }

    assertEquals(List(" First", " Second", "Third", ""), docstring)
  }
}
