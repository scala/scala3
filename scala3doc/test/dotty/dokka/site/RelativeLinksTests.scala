package dotty.dokka.site

import org.junit.Test
import org.junit.Assert._

class RelativePathsLink:

  @Test
  def testLinks() =
    def path(from: String, to: String) =
      relativePath(from.split('/').toList, to.split('/').toList)

    assertEquals(relativePath(Nil, Seq("a", "b")), "a/b")

    assertEquals(
      path("api/dotty/dokka/tasty/comments/wiki", "api/dotty/dokka/tasty/comments"),
      "../comments"
    )

    assertEquals(
      path("api/dotty/dokka/tasty/comments/wiki", "api/index"),
      "../../../../index"
    )

