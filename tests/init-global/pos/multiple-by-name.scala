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

  def foo2(q: => X) = foo(q)
  def foo3(r: => X) = foo(r)

  val a = foo(new X)
  val b = foo(new Y)
  val c = foo2(new Y)
  val d = foo3(new Y)
}
