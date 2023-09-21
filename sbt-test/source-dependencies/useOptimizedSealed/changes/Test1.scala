class Test {
  def foo(x: Sealed): Int = x match
    case _: Child1 => 1
    case _: Child2 => 1

  val s: Sealed = new Child1
}
