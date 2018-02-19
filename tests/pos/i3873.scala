object Test {
  inline def sum2(ys: List[Int]): Unit = {
    ys.foldLeft(1)
  }
  val h1: ((List[Int]) => Unit) = sum2
}
