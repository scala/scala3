object testRepeated {
  def foo = java.lang.System.out.format("%4$2s %3$2s %2$2s %1$2s", "a", "b", "c", "d")

  def bar(xs: Int*): Int = xs.length
  def bat(xs: scala.Seq[Int]): Int = xs.length

  def test(xs: List[Int]): Unit = {
    bar(1, 2, 3)
    bar(xs: _*)

    val List(_, ys: _*) = xs
    bar(ys: _*)
    bat(ys)
  }
}
