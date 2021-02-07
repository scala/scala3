object Test {
  List(1, 2, 3, 4) match {
    case List(1, 2, xs*) =>
      val ys: Seq[Int] = xs
      println(ys)
  }

  val List(1, 2, x*) = List(1, 2, 3, 4)
}
