// FIXME: changes to workaround type avoidance bugs
object Test extends App {
  def foo1(x: AnyRef): Any = x match { case x: Function0[a] => x() }
  def foo2(x: AnyRef) = x match { case x: Function0[Any] => x() }
}
