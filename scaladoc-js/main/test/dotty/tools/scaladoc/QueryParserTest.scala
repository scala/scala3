package dotty.tools.scaladoc

import org.junit.{Test, Assert}
import org.junit.Assert._

class QueryParserTest:
  val queryParser = QueryParser()
  val kinds = Seq(
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
  private def testCase(query: String, result: EngineQuery) = {
    val parsed = queryParser.parse(query)
    assertEquals(
      s"Query parser test error: for query: $query expected $result but found $parsed",
      parsed,
      result
    )
  }

  @Test
  def queryParserTests() = {
    kinds.foreach(k => testCase(s"$k ", EngineMatchersQuery(List(ByKind(k), ByName("")))))
    testCase("trait", EngineMatchersQuery(List(ByName("trait"))))
    testCase("trait A", EngineMatchersQuery(List(ByKind("trait"), ByName("A"))))
    testCase("`trait A`", EngineMatchersQuery(List(ByName("trait A"))))
  }
