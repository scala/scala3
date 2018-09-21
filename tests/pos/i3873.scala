object Test {
  inline def sum2(ys: List[Int]): Unit = {
    ys.foldLeft(1)
  }
  val h1 = (xs: List[Int]) => sum2(xs)
}
