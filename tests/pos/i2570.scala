object Test {

  def repeat(s: String, i: Int, j: Int = 22) = s * i

  val f1 = repeat("abc", _)
  val f2: Int => String = f1
  val f3 = repeat(_, 3)
  val f4: String => String = f3
  val f5 = repeat("abc", _, _)
  val f6: (Int, Int) => String = f5
  val f7 = repeat(_, 11, _)
  val f8: (String, Int) => String = f7

  def sum(x: Int, y: => Int) = x + y

  val g1 = sum(2, _)
  val g2: (=> Int) => Int = g1

  val h0: ((Int, => Int) => Int) = sum

  def sum2(x: Int, ys: Int*) = (x /: ys)(_ + _)
  val h1: ((Int, Seq[Int]) => Int) = sum2

// Not yet:
//  val h1 = repeat
//  val h2: (String, Int, Int) = h1
//  val h3 = sum
//  val h4: (Int, => Int) = h3
}
