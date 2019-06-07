package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

class TestWhitelistedCollections extends DottyDocTest with CheckFromSource {

  @Test def arrayAndImmutableHasDocumentation =
    checkFiles(TestWhitelistedCollections.files) { (ctx, packages) =>
      val array =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.Array")
        .get

      assert(array.comment.get.body.length > 0,
        "scala.Array didn't have any documentation")

      val imm =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.Immutable")
        .get

      assert(
        imm.kind == "trait" && imm.name == "Immutable",
        "Found wrong `Immutable`")
      assert(
        imm.comment.map(_.body).get.length > 0,
        "Imm did not have a comment with length > 0")
    }
}

object TestWhitelistedCollections {
  val files: List[String] =
    TestSources.stdLibSources
    .filterNot(_.endsWith("package.scala"))
}
