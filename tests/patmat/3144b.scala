class Test {
  def f(x: Any): Int = x match {
    case xs: List[Int] @unchecked => xs.head
    case _ => 0
  }
}
