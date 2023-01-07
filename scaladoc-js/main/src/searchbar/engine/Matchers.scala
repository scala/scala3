package dotty.tools.scaladoc

case class MatchResult(score: Int, pageEntry: PageEntry, indices: Set[Int])

sealed trait EngineQuery
case class NameAndKindQuery(name: Option[String], kind: Option[String]) extends EngineQuery

case class SignatureQuery(signature: String) extends EngineQuery
