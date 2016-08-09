package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

class TestWhitelistedCollections extends DottyTest {
  @Test def arrayHasDocumentation =
    checkFiles(WhitelistedStandardLib.files) { doc =>
      val array = doc
        .packages("scala")
        .children.find(_.path.mkString(".") == "scala.Array")
        .get

      assert(array.comment.get.body.length > 0)
    }

  @Test def traitImmutableHasDocumentation =
    checkFiles(WhitelistedStandardLib.files) { doc =>
      val imm = doc
        .packages("scala")
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
