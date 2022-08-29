package dotty.tools.scaladoc

import math.Ordering.Implicits.seqOrdering
import org.scalajs.dom.Node

import scala.annotation.tailrec

class PageSearchEngine(pages: List[PageEntry]):
  def query(query: NameAndKindQuery): List[MatchResult] =
    matchPages(query)
      .filter {
        case MatchResult(score, _, _) => score >= 0
      }
      .sortBy {
        case MatchResult(score, _, _) => score
      }

  private def matchPages(query: NameAndKindQuery): List[MatchResult] = query match {
    case NameAndKindQuery(None, None) => List.empty
    case NameAndKindQuery(None, Some(kind)) =>
      filterKind(pages, kind)
        .map(MatchResult(1, _, Set.empty))
    case NameAndKindQuery(Some(""), kind) =>
      kind.fold(pages)(filterKind(pages, _))
    case NameAndKindQuery(Some(nameSearch), kind) =>
      val kindFiltered = kind.fold(pages)(filterKind(pages, _))
      val prematchedPages = prematchPages(kindFiltered, nameSearch)
      prematchedPages.map(matchPage(_, nameSearch))
  }

  private def matchPage(prematched: MatchResult, nameSearch: String): MatchResult =
    val searchTokens = StringUtils.createCamelCaseTokens(nameSearch)
    val pageTokens = page.tokens

    @tailrec
    def findHighScoreMatch(searchTokens: List[String], pageTokens: List[String], scoreAcc: Int, indicesAcc: Set[Int]): (Int, Set[Int]) =
      val searchTokensLifted = searchTokens.lift
      val pageTokensLifted = pageTokens.lift

      // Instead znajdź wszystkie poprawne podciągi match ujących tokenów OR leave it?
      @tailrec
      def matchTokens(searchTokenIndex: Int, pageTokenIndex: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] = (searchTokensLifted(searchTokenIndex), pageTokensLifted(pageTokenIndex)) match
        case (None, _) || (_, None) => acc
        case (Some(searchHead :: _), Some(pageHead :: _)) =>
          if searchHead.head == pageHead.head then
            matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc + (searchTokenIndex, pageTokenIndex))
          else
            matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc)
      end matchTokens

      val matchedTokens = matchTokens(0, 0, Set.empty)


      def findHighScoreMatch(searchTokenIndex: Int, pageTokenIndex: Int, positionAcc: Set[Int], scoreAcc: Int) =



  private def filterKind(pages: List[PageEntry], kind: String): List[PageEntry] =
    pages.filter(_.kind == kind)

  def prematchPages(pages: List[PageEntry], search: String): List[PageEntry]
    pages.map(page => MatchResult(1, page, getIndicesOfSearchLetters(page.shortName, search)))
      .filter(_.indices.nonEmpty)

  private def getIndicesOfSearchLetters(pageName: String, search: String): Set[Int] =
    @tailrec
    def getIndicesAcc(nameIndex: Int, searchIndex: Int, acc: Set[Int]): Set[Int] =
      if nameIndex >= pageName.length then
        Set.empty
      else if searchIndex >= search.length then
        acc
      else if pageName(nameIndex) == search(searchIndex) then
        containsAllAcc(nameIndex + 1, searchIndex + 1, acc + nameIndex)
      else
        containsAllAcc(nameIndex + 1, searchIndex, acc)
    getIndicesAcc(0, 0)

  }