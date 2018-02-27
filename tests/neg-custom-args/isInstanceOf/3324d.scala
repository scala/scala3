class Test {
  val x: Any = ???

  x match {
    case _: List[Int @unchecked] => 5
  }

  def foo(x: Any): Boolean =
    x.isInstanceOf[List[String]]  // error
}