package dotty.dokka
package renderers

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import dotty.dokka.HTML._

class LocationTests:
  given DocContext = testDocContext
  object locations extends Locations:
    val members = Map.empty

  @Test
  def testPathToRoot() =
    assertEquals("../root.png", locations.resolveRoot(Seq("a", "b"), "root.png"))
    assertEquals("root.png", locations.resolveRoot(Seq("ala.html"), "root.png"))
    assertEquals("c/root.png", locations.resolveRoot(Seq("a", "b", "index"), "a/b/c/root.png"))

  @Test
  def testLinks() =
    def path(from: String, to: String) =
      locations.pathTo(to.split('/').toList, from.split('/').toList)

    assertEquals("a/b", locations.pathTo(Seq("a", "b"), Nil))

    assertEquals(
      "../comments",
      path("api/dotty/dokka/tasty/comments/wiki", "api/dotty/dokka/tasty/comments"),
    )

    assertEquals(
      "../../../../index",
      path("api/dotty/dokka/tasty/comments/wiki", "api/index"),
    )

    assertEquals(
      "../../annotation",
      path("api/scala/annotation/meta/beanGetter", "api/scala/annotation"),
    )