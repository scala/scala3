object Test with
  val xs = List(1, 2, 3, 4, 5)
  xs match {
    case List(1, 2, xs1 @ xs2: _*) => println(xs2) // error // error
    case _ => ()
  }