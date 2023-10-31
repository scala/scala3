class Test {
  def f(a: Array[?]|Null): Unit  = a match {
   case x: Array[Int] =>
  }
}
