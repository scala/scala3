package dotty.tools.scaladoc

sealed trait EngineQuery
case class EngineMatchersQuery(matchers: List[Matchers]) extends EngineQuery
case class BySignature(signature: String) extends EngineQuery

sealed trait Matchers extends Function1[PageEntry, Int]

case class ByName(query: String) extends Matchers:
  val tokens = StringUtils.createCamelCaseTokens(query)
  def apply(p: PageEntry): Int = {
    val nameOption = Option(p.shortName.toLowerCase)
    //Edge case for empty query string
    if query == "" then 1
    else {
      val results = List(
        nameOption.filter(_.contains(query.toLowerCase)).fold(-1)(_.size - query.size),
        if p.tokens.size >= tokens.size && p.tokens.zip(tokens).forall( (token, query) => token.startsWith(query))
          then p.tokens.size - tokens.size + 1
          else -1
        //acronym.filter(_.contains(query)).fold(-1)(_.size - query.size + 1)
      )
      if results.forall(_ == -1) then -1 else results.filter(_ != -1).min
    }
  }

case class ByKind(kind: String) extends Matchers:
  def apply(p: PageEntry): Int = if p.kind.equalsIgnoreCase(kind) then 1 else -1
