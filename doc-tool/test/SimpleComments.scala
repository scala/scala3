package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

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
        .flatMap(_.comment.map(_.body))
        .get

      assertEquals(traitCmt, "<p>Hello, world!</p>")
    }
  }

  @Test def commentOnPackageObject = {
    val source =
      """
      |/** Hello, world! */
      |package object foobar { class A }
      """.stripMargin

    checkSource(source) { packages =>
      val packageCmt = packages("foobar").comment.get.body
      assertEquals("<p>Hello, world!</p>", packageCmt)
    }
  }
}
