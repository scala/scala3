class C
type Cap = C^

class A
class B

class Foo(x: Cap):

  def foo: A ->{cap[Foo]} Unit = ???

  class Bar(y: Cap):

    def bar: B ->{cap[Bar]} Unit = ???

    def f(a: A ->{cap[Foo]} Unit, b: B ->{cap[Bar]} Unit)
      : (A ->{a} Unit, B ->{b} Unit)
      = (a, b)

def test(c1: Cap, c2: Cap) =
  val x = Foo(c1)
  val y = x.Bar(c2)
  val xfoo = x.foo
  val ybar = y.bar
  val z1 = y.f(xfoo, ybar)
  val z2 = y.f(x.foo, y.bar)
  ()
