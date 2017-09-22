class Test {
  def f(x: Any): Int = x match {
    case xs: List[Int] @unchecked => xs.head
    case xs: Array[List[Int]] => 3
    case _ => 0
  }

  def g(x: Any): Int = x match {
    case xs: List[Int @unchecked] => xs.head
    case xs: Array[List[Int]] => 3
    case _ => 0
  }
}
