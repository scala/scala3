package dotty.tools.scaladoc

case class MatchResult(score: Int, pageEntry: PageEntry, indices: Set[Int])

sealed trait EngineQuery
case class NameAndKindQuery(name: Option[String], kind: Option[String]) extends EngineQuery:
  
case class SignatureQuery(signature: String) extends EngineQuery

case class Match(priority: Int, matchedIndexes: Set[Int]) // matchedIndexes - indexes of chars that got matched

sealed trait Matchers:
  def matchEntry(p: PageEntry): Match

/**
 * Searches matches PageEntry by name. It implements the following logic:
 *  1. When the query is empty, return successful match regardless of PageEntry.
 *  2.
 * @param query String to match in page names
 */
case class ByName(query: String) extends Matchers

case class ByKind(kind: String) extends Matchers:
  def apply(p: PageEntry): Match = Match(if p.kind.equalsIgnoreCase(kind) then 1 else -1, Set.empty)
