package dotty.tools.scaladoc
package renderers

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import dotty.tools.scaladoc.util.HTML._

class LocationTests:
  given DocContext = testDocContext()
  object locations extends Locations:
    val effectiveMembers = Map.empty

  @Test
  def testPathToRoot() =
    assertEquals("../root.png", locations.resolveRoot(Seq("a", "b"), "root.png"))
    assertEquals("root.png", locations.resolveRoot(Seq("ala.html"), "root.png"))
    assertEquals("c/root.png", locations.resolveRoot(Seq("a", "b", "index"), "a/b/c/root.png"))

  @Test
  def testLinks() =
    def path(from: String, to: String) =
      locations.pathToRaw(from.split('/').toList, to.split('/').toList)

    assertEquals("a/b", locations.pathToRaw(Nil, Seq("a", "b")))

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

  @Test
  def testAnchorLinks() =
    def pathWithAnchor(location: String, anchor: String) =
      locations.escapedAbsolutePathWithAnchor(new DRI(location, anchor))

    assertEquals(
      "scala/%23::.html#abcde",
      pathWithAnchor("scala.#::", "abcde")
    )

    assertEquals(
      "scala/collection/immutable/LazyList$$%23$.html#abcde",
      pathWithAnchor("scala.collection.immutable.LazyList$$#$", "abcde")
    )
