object Test extends App {
  def foo1(x: AnyRef) = x match { case x: Function0[?] => x() }
  def foo2(x: AnyRef) = x match { case x: Function0[Any] => x() }
}
