object Test {
  inline def sum2(ys: List[Int]): Int = (1 /: ys)(_ + _)
  val h1 = (xs:  List[Int]) => sum2(xs)
}
