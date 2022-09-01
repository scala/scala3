package dotty.tools.scaladoc

import math.Ordering.Implicits.seqOrdering
import org.scalajs.dom.Node

import scala.annotation.tailrec

/**
 * TODO:
 * - Prematcher simple scoring
 * - Test first token score
 * - Maybe matcher for len > 1?
 * - Fix kinds (class List)
 * - Search for docs in Docs, for classes/etc. in Api.
 * - Write tests! Lists of pages and assert ordering.
 * - Optimize.
 */
class PageSearchEngine(pages: List[PageEntry]):
  def query(query: NameAndKindQuery): List[MatchResult] =
    println("QUERYING: " + query)
    matchPages(query)
      .filter {
        case MatchResult(score, _, _) => score >= 0
      }
      .sortBy {
        case MatchResult(score, _, _) => -score
      }

  private def kindScoreBonus(kind: String): Int = kind match {
    case "class"                          => 5
    case "object" | "enu,"                => 4
    case "trait"                          => 3
    case "def" | "val" | "given" | "type" => 1
    case _                                => 0
  }

  private def matchCompletnessBonus(nameCharacters: Int, matchCharacters: Int): Int =
    (matchCharacters * 3) / nameCharacters

  private def matchPages(query: NameAndKindQuery): List[MatchResult] = query match
    case NameAndKindQuery(None, None) => List.empty
    case NameAndKindQuery(None, Some(kind)) =>
      filterKind(pages, kind)
        .map(MatchResult(1, _, Set.empty))
    case NameAndKindQuery(Some(""), kind) =>
      kind.fold(pages)(filterKind(pages, _))
        .map(MatchResult(1, _, Set.empty))
    case NameAndKindQuery(Some(nameSearch), kind) =>
      val kindFiltered = kind.fold(pages)(filterKind(pages, _))
      println("PREMATCHING: " + nameSearch)
      val prematchedPages = prematchPages(kindFiltered, nameSearch)
      println("PREMATCHED " + prematchedPages.length)
      var totalMatched = 0

      if nameSearch.length > 2 then
        prematchedPages.map(prematched =>
          val finalMatch = matchPage(prematched, nameSearch)
          val bonusScore = kindScoreBonus(prematched.pageEntry.kind)
            + matchCompletnessBonus(prematched.pageEntry.shortName.length, nameSearch.length)
          finalMatch.copy(score = finalMatch.score + bonusScore)
        )
      else prematchedPages


  private def filterKind(pages: List[PageEntry], kind: String): List[PageEntry] =
    pages.filter(_.kind == kind)

  def prematchPages(pages: List[PageEntry], search: String): List[MatchResult] =
    pages.map(page => MatchResult(1, page, getIndicesOfSearchLetters(page.shortName, search)))
      .filter(_.indices.nonEmpty)

  val debuggedTypes = Set("list", "lazylist", "classmanifest")

  private def getIndicesOfSearchLetters(pageName: String, search: String): Set[Int] =
    if debuggedTypes.contains(pageName) then println("Prematching List!")
    @tailrec
    def getIndicesAcc(nameIndex: Int, searchIndex: Int, acc: Set[Int]): Set[Int] =
      if searchIndex >= search.length then
        if debuggedTypes.contains(pageName) then println("Matched!")
        acc
      else if nameIndex >= pageName.length then
        if debuggedTypes.contains(pageName) then println("Not matched :(")
        Set.empty
      else if pageName(nameIndex).toLower == search(searchIndex).toLower then
        if debuggedTypes.contains(pageName) then println("Matched name:" + nameIndex + "(" + pageName(nameIndex) +  ") with search:" + searchIndex + "(" + search(searchIndex) + ")")
        getIndicesAcc(nameIndex + 1, searchIndex + 1, acc + nameIndex)
      else
        if debuggedTypes.contains(pageName) then println("Not matched: " + nameIndex + "(" + pageName(nameIndex) +  ") with search:" + searchIndex + "(" + search(searchIndex) + ")")
        getIndicesAcc(nameIndex + 1, searchIndex, acc)
    getIndicesAcc(0, 0, Set.empty)

  private def matchPage(prematched: MatchResult, nameSearch: String): MatchResult =
    val searchTokens: List[List[Char]] = StringUtils.createCamelCaseTokens(nameSearch).map(_.toList) //todo extract
    val pageTokens: List[List[Char]] = prematched.pageEntry.tokens.map(_.toList)
    val pageName = prematched.pageEntry.shortName
    if debuggedTypes.contains(pageName) then println("Found " + pageName + "!")
    val searchTokensLifted = searchTokens.lift
    val pageTokensLifted = pageTokens.lift

    @tailrec
    def matchTokens(searchTokenIndex: Int, pageTokenIndex: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] =
      (searchTokensLifted(searchTokenIndex).map(_.toList), pageTokensLifted(pageTokenIndex).map(_.toList)) match
        case (None, _) | (_, None) => acc
        case (Some(searchHead :: _), Some(pageHead :: _)) =>
          if searchHead == pageHead then
            matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc + ((searchTokenIndex, pageTokenIndex)))
          else
            matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc)
        // empty tokens edge cases
        case (Some(_), Some(_ :: _)) => matchTokens(searchTokenIndex + 1, pageTokenIndex, acc)
        case (Some(_ :: _), Some(_)) => matchTokens(searchTokenIndex, pageTokenIndex + 1, acc)
        case _                       => matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc)
    end matchTokens

    val matchedTokens = matchTokens(0, 0, Set.empty)
    val searchTokenPositions = searchTokens.map(_.length).scanLeft(0)(_ + _)
    val pageTokensPositions = pageTokens.map(_.length).scanLeft(0)(_ + _)
    if debuggedTypes.contains(pageName) then
      println("List tokens: " + matchedTokens)
      println("Search: " + searchTokenPositions)
      println("Page: " + pageTokensPositions)


    @tailrec
    def findHighScoreMatch(
        searchTokenIndex: Int,
        searchPosition: Int,
        pageTokenIndex: Int,
        pagePosition: Int,
        positionAcc: Set[Int],
        scoreAcc: Int,
        consecutiveMatches: Int
      ): Option[MatchResult] =
      if searchPosition >= nameSearch.length then
          if debuggedTypes.contains(pageName) then println("Matched " + pageName + " with score " + scoreAcc)
          Some(MatchResult(scoreAcc, prematched.pageEntry, positionAcc))
      else if pagePosition >= pageName.length then
        if debuggedTypes.contains(pageName) then println("Failed to match " + pageName + " :(")
        None
      else
        if debuggedTypes.contains(pageName) then
          println("At page: " + pageTokenIndex + "/" + pagePosition + "(" + pageName(pagePosition) + ")" + "; search: " + searchTokenIndex + "/" + searchPosition + "(" + nameSearch(searchPosition) + ")")
        val currentSearchTokenStart = searchTokenPositions(searchTokenIndex)
        val currentPageTokenStart = pageTokensPositions(pageTokenIndex)
        val atSearchTokenBeggining = searchPosition == currentSearchTokenStart
        val matchingPageToken = matchedTokens.find(_._1 == currentSearchTokenStart).map(_._2)
        val searchChar = nameSearch.charAt(searchPosition).toLower
        val pageChar = pageName.charAt(pagePosition).toLower

        def recalculateTokenIndex(tokenPositions: Seq[Int], lastIndex: Int, position: Int): Int =
          if tokenPositions.length <= lastIndex + 1 || tokenPositions(lastIndex + 1) > position then
            lastIndex
          else
            lastIndex + 1

        val positionScores = List(8,4,2,1).orElse(PartialFunction.fromFunction(_ => 0))
        def getMatchScore(matchedPagePosition: Int, matchedPageTokenStart: Int): Int =
          val consecutiveMatchesScore = if consecutiveMatches > 0 then 1 else 0
          val matchPositionScore = positionScores(matchedPagePosition - matchedPageTokenStart)
          val firstTokenScore = if matchPositionScore > 0 && matchedPageTokenStart == 0 then 3 else 0
          if debuggedTypes.contains(pageName) then println("[" + pageName + "] + score " + (consecutiveMatchesScore + matchPositionScore))
          consecutiveMatchesScore + matchPositionScore + firstTokenScore

        matchingPageToken match
          case Some(matchingToken) if searchPosition == currentSearchTokenStart =>
            val matchedTokenPosition = pageTokensPositions(matchingToken)
            if debuggedTypes.contains(pageName) then println("Matched tokens! " + matchingToken + " at " + matchedTokenPosition)
            findHighScoreMatch(
              recalculateTokenIndex(searchTokenPositions, searchTokenIndex, searchPosition + 1),
              searchPosition + 1,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, matchedTokenPosition + 1),
              matchedTokenPosition + 1,
              positionAcc + matchedTokenPosition,
              scoreAcc + getMatchScore(matchedTokenPosition, matchedTokenPosition),
              consecutiveMatches + 1
            )
          case _  if searchChar == pageChar =>
            if debuggedTypes.contains(pageName) then println("Matched char! " + searchChar + " at " + pagePosition)
            val matchedTokenPosition = matchingPageToken.map(pageTokensPositions).getOrElse(0)
            findHighScoreMatch(
              recalculateTokenIndex(searchTokenPositions, searchTokenIndex, searchPosition + 1),
              searchPosition + 1,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, pagePosition + 1),
              pagePosition + 1,
              positionAcc + pagePosition,
              scoreAcc + getMatchScore(pagePosition, matchedTokenPosition),
              consecutiveMatches + 1
            )
          case _ =>
            findHighScoreMatch(
              searchTokenIndex,
              searchPosition,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, pagePosition + 1),
              pagePosition + 1,
              positionAcc,
              scoreAcc,
              0
            )

    val highScoreMatch = findHighScoreMatch(0, 0, 0, 0, Set.empty, 0, 0)
    highScoreMatch.getOrElse(prematched)






