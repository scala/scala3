class Lang(name: String)
object Lang {
  val Default = Lang("")
  def apply(language: String): Lang = ???
  def apply(maybeLang: Option[String], default: Lang = Default): Lang = ???
}
