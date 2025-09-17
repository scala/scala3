class X {
  def bar(): Int = 5
}
class Y extends X {
  override def bar(): Int = 6
}

object O {
  def foo(p: => X) = {
    p.bar()
  }

  val a = foo(new X)
  val b = foo(new Y)
}