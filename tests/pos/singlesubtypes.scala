object E {
  val a: String = ???
  val b: String = ???
}

object Test {

  val a: E.a.type = E.a
  val b: E.b.type = E.b

  val c: a.type | b.type = ???
  val d: a.type | b.type = c
}
