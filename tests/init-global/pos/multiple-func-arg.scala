class X {
  def bar(): Int = 5
}
class Y extends X {
  override def bar(): Int = 6
}

object O {
  def foo(p: () => X) = {
    p().bar() // flow: p <- [Fun(() => new X), Fun(() => new Y), Fun(() => q()), Fun(() => r())]
  }

  def foo2(q: () => X) = foo(() => q()) // flow: q <- [Fun(() => new Y)]
  def foo3(r: () => X) = foo(() => r()) // flow: r <- [Fun(() => new Y)]

  val a = foo(() => new X)
  val b = foo(() => new Y)
  val c = foo2(() => new Y)
  val d = foo3(() => new Y)
}