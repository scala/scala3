package dotty.tools.scaladoc

sealed trait EngineQuery
case class EngineMatchersQuery(matchers: List[Matchers]) extends EngineQuery
case class BySignature(signature: String) extends EngineQuery

case class Match(priority: Int, matchedIndexes: Set[Int]) // matchedIndexes - indexes of chars that got matched

sealed trait Matchers extends Function1[PageEntry, Match]

case class ByName(query: String) extends Matchers:
  val tokens = StringUtils.createCamelCaseTokens(query)
  def apply(p: PageEntry): Match = {
    val nameOption = Option(p.shortName.toLowerCase)
    //Edge case for empty query string
    if query == "" then Match(1, Set.empty)
    else {
      val (result, indexes) = p.shortName.toLowerCase.zipWithIndex.foldLeft((query.toLowerCase, Set.empty[Int])) {
        case ((pattern, indexes), (nextChar, index)) =>
          if !pattern.isEmpty then {
            if pattern.head.toString.equalsIgnoreCase(nextChar.toString) then (pattern.tail, indexes + index) else (pattern, indexes)
          } else ("", indexes)
      }
      if result.isEmpty then Match(p.shortName.size - query.size + 1, indexes) else Match(-1, Set.empty)
    }
  }

case class ByKind(kind: String) extends Matchers:
  def apply(p: PageEntry): Match = Match(p.fullName.split(" ").headOption.filter(_.equalsIgnoreCase(kind)).fold(-1)(_ => 1), Set.empty)
