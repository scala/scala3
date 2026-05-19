class DiscordRecord(fields: Map[String, Any]) extends Selectable {
  def selectDynamic(name: String): Any = fields(name)
}

type IdentifyScope = DiscordRecord {
  val username: String
}

type IncludeIf[Stated <: String, Requirement <: String, IfMatch] = Seq[Stated] match {
  case Seq[Requirement] => IfMatch
  case _ => Any
}

type RawUser[Scopes <: String] = DiscordRecord & IncludeIf[Scopes, "identify", IdentifyScope]

val obj: RawUser["identify"] = ???
val _ = obj.username
