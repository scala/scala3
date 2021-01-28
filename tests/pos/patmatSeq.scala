object Test {

  val xs = List(1, 2, 3)

  (xs: Any) match {
    case Seq(x, y) => println(s"$x, $y")
    case Seq(x*) => println(s"other sequence")
    case _ => println("None")
  }

}
