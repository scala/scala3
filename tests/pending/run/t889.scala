object Test extends dotty.runtime.LegacyApp {

  val a = List("a")

  a match {
    case Seq("a", "b", rest : _*) => println("a, b, " + rest)
    case Seq(first, rest : _*) => println("first: " + first + ", rest: " + rest)
  }
}
