object Test extends App {
  val Array(who, what*) = "first second third" split (" ")
  println(what)
}
