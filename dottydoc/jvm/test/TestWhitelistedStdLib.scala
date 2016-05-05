package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

class TestWhitelistedCollections extends DottyTest {
  @Test def arrayHasDocumentation =
    checkCompile(WhitelistedStandardLib.files) { doc =>
      val array = doc
        .packages("scala")
        .children.find(_.path.mkString(".") == "scala.Array")
        .get

      assert(array.comment.get.body.length > 0)
    }
}
