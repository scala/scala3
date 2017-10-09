package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotty.uoption._

class TestSimpleComments extends DottyDocTest {

  @Test def simpleComment = {
    val source =
      """
      |package scala
      |
      |/** Hello, world! */
      |trait HelloWorld
      """.stripMargin

    checkSource(source) { packages =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body).toOption)
        .get

      assertEquals(traitCmt, "<p>Hello, world!</p>")
    }
  }
}
