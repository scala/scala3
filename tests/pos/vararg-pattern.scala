object Test {
  List(1, 2, 3, 4) match {
    case List(1, 2, xs: _*) =>
      val ys: Seq[Int] = xs
      println(ys)
  }

  val List(1, 2, x: _*) = List(1, 2, 3, 4)
}
