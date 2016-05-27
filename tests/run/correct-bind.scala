object Test extends dotty.runtime.LegacyApp {
  val Array(who, what: _*) = "first second third" split (" ")
  println(what)
}
