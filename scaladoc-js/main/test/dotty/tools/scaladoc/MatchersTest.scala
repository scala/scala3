package dotty.tools.scaladoc

import org.junit.{Test, Assert}
import org.junit.Assert._

class MatchersTest:
  private val kinds = Seq(
    "class",
    "trait",
    "enum",
    "object",
    "def",
    "val",
    "var",
    "package",
    "given",
    "type"
  )
  private val names = Seq(
    "NullPointerException",
    "NPException",
    "Seq",
    "SeqOps",
    "writeBytes",
    "lessOrEqual",
    "testFuzzySearch1",
    "testF",
    "testFS"
  )
  private val pages = for {
    kind <- kinds
    name <- names
  } yield PageEntry(
    s"$kind $name",
    "",
    "",
    "",
    false,
    s"$name",
    kind,
    StringUtils.createCamelCaseTokens(name)
  )

  private def result(matchers: List[Matchers]) = {
    pages.map { p =>
      p -> matchers.map(_(p))
    }.filterNot { (page, results) =>
      results.exists(r => r.priority == -1)
    }.map((page, results) => page)
  }

  @Test
  def testByKind = kinds.foreach { kind =>
    val res = result(List(ByKind(kind)))
    val expected = pages.filter(p => p.fullName.startsWith(kind)).toSet
    assertEquals(
      s"Matchers test error: for kind: $kind should match $expected but matched $res",
      expected,
      res.toSet
    )
  }

  private def byNameTestCase(query: String, expectedMatch: String*) = expectedMatch.foreach { expMatch =>
      assertTrue(
        s"Matchers test error: for query: $query expected $expMatch",
        result(List(ByName(query))).exists(p => p.shortName.contains(expMatch))
      )
    }

  @Test
  def testByName = {
    names.foreach(n => byNameTestCase(n, n))
    byNameTestCase("NPE", "NPException", "NullPointerException")
    byNameTestCase("NullPE", "NullPointerException")
    byNameTestCase("tFuzzS", "testFuzzySearch1")
    byNameTestCase("SO", "SeqOps")
    byNameTestCase("teFS", "testFS")
    byNameTestCase("writeBy", "writeBytes")
    byNameTestCase("seQ", "Seq")
    byNameTestCase("lOrEqu", "lessOrEqual")
    byNameTestCase("teF", "testFS", "testF")
  }
