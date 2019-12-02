object Test extends App {
  val Array(who, what: _*) = "first second third" split (" ")
  println(what)
}
