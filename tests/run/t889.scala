object Test extends App {

  val a = List("a")

  a match {
    case Seq("a", "b", rest *) => println("a, b, " + rest)
    case Seq(first, rest *) => println("first: " + first + ", rest: " + rest)
  }
}
