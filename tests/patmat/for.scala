object Test {
  def foo[A, B](l: List[(A, B)]): List[A] = {
    for ((a, b) <- l) yield a
  }

  def bar(xs: List[(Int, List[Int])]): Unit = for (case (_, x :: y :: xs) <- xs) yield x
}