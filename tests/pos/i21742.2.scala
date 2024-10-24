case class C(n: Int, ds: Seq[Double])
class Test:
  def m(using n: Int): Int = n + 1
  def t(): Unit =
    C(1, Seq(2, 3, 4)) match { case C(given Int, ds) => m }
