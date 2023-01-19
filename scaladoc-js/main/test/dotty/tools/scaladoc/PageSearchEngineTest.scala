package dotty.tools.scaladoc

import org.junit.{Assert, Test}
import org.junit.Assert.*

import scala.concurrent.Await
import scala.concurrent.duration.*

class PageSearchEngineTest {

  def page(kind: String, name: String) = PageEntry(
      s"$kind $name",
      "",
      "",
      "",
      "",
      false,
      s"$name",
      kind,
      StringUtils.createCamelCaseTokens(name)
    )

  case class ExpectedMatch(kind: String, name: String, indices: Set[Int])
  def assertMatches(query: NameAndKindQuery, pages: List[PageEntry], matches: List[String]): Unit =
    val expectedMatches = matches.map { mat =>
      val splitResult = mat.split(" ")
      val kind = splitResult(0)
      val name = splitResult.tail.mkString(" ")
      val (realName, indices, _) = name.foldLeft[(String, Set[Int], Boolean)]("", Set.empty, false) {
        case ((name, matchIndices, inParam), c) =>
          val index = name.length
          if c == '(' then
            if inParam then
              throw new IllegalArgumentException("Nested params not allowed")
            else
              (name, matchIndices, true)
          else if c == ')' then
            (name, matchIndices, false)
          else if inParam then
            (name + c, matchIndices + index, true)
          else
            (name + c, matchIndices, false)
      }
      ExpectedMatch(kind, realName, indices)
    }
    val engine = new PageSearchEngine(pages)
    val resultingMatches = engine.query(query)
      .map(mat => ExpectedMatch(mat.pageEntry.kind, mat.pageEntry.shortName, mat.indices))

    val matchesNames = resultingMatches.map(s => (s.name, s.kind))
    val expectedNames = expectedMatches.map(s => (s.name, s.kind))
    val missingNames = expectedNames.diff(matchesNames)
    val extraNames = matchesNames.diff(expectedNames)
    val itemsNotMatchingNames = (resultingMatches.diff(expectedMatches) ++ expectedMatches.diff(resultingMatches))
      .filter(m => !(missingNames ++ extraNames).contains((m.name, m.kind))).map(s => (s.name, s.kind))
    val itemsNotMatching = itemsNotMatchingNames.map {
      case pair @ (itemName, itemKind) =>
        val expectedItem: ExpectedMatch = expectedMatches.find(s => (s.name, s.kind) == pair).get
        val matchedItem: ExpectedMatch = resultingMatches.find(s => (s.name, s.kind) == pair).get
        s"${itemKind} ${itemName}: ${expectedItem.indices.toList.sorted.mkString("[", ", ", "]")} vs ${matchedItem.indices.toList.sorted.mkString("[", ", ", "]")}"
    }.mkString("\n")

    assertTrue(
      s"\nFound: ${matchesNames.mkString("[", ", ", "]")} \n" +
        s"Expected: ${expectedNames.mkString("[", ", ", "]")} \n" +
      s"Extra elements: ${extraNames.mkString(", ")} \n" +
      s"Missing elements: ${missingNames.mkString(", ")}\n" +
      s"Not matching items: \n${itemsNotMatching}\n",
      resultingMatches == expectedMatches
    )


  private val correctFilterPages = List(
    page("class", "ListBuffer"),
    page("object", "ListBuffer"),
    page("class", "ListBuff"),
    page("class", "LisBfufer"),
    page("class", "ListBufferTwo"),
    page("class", "ListerBuffer")
  )
  @Test
  def correctFilter(): Unit = {
    assertMatches(
      NameAndKindQuery(Some("ListBuffer"), Some("class")),
      correctFilterPages,
      List(
        "class (ListBuffer)",
        "class (List)er(Buffer)",
        "class (ListBuffer)Two",
      )
    )
  }

  private val abbrevFilterPages = List(
    page("class", "NullPointerException"),
    page("class", "NullBointerException"),
    page("class", "NullBpointerException"),
    page("class", "nullpointerexception"),
  )
  @Test
  def abbrevFilter(): Unit = {
    assertMatches(
      NameAndKindQuery(Some("NPE"), Some("class")),
      abbrevFilterPages,
      List(
        "class (N)ull(P)ointer(E)xception",
        "class (N)ullB(p)oint(e)rException",
        "class (n)ull(p)oint(e)rexception",
      )
    )
  }

  private val correctOrderPages = List(
    page("class", "ListBuffer"),
    page("object", "ListBuffer"),
    page("static", "Using List Buffers"),
    page("class", "ListUnbucle"),
    page("object", "Malibu")
  )
  @Test
  def correctOrder(): Unit = {
    assertMatches(
      NameAndKindQuery(Some("LiBu"), None),
      correctOrderPages,
      List(
        "class (Li)st(Bu)ffer",
        "object (Li)st(Bu)ffer",
        "static Using (Li)st (Bu)ffers",
        "class (Li)stUn(bu)cle",
        "object Ma(libu)"
      )
    )
  }

  private val correctSelectionPages = List(
    page("class", "FoobarBar"),
    page("class", "FooBbar"),
    page("class", "FobaroBar")
  )

  @Test
  def correctSelection(): Unit = {
    assertMatches(
      NameAndKindQuery(Some("FooBar"), None),
      correctSelectionPages,
      List(
        "class (Foo)bar(Bar)",
        "class (FooB)b(ar)",
        "class (Fo)bar(oBar)"
      )
    )
  }
}
