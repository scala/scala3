object Test {
  def foo[A, B](l: List[(A, B)]): List[A] = {
    for ((a, b) <- l) yield a
  }
}