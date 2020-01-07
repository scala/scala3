class A
class B extends A

extension a of (x: A) with
  def foo: Int = 1

extension b of (x: B) with
  def foo: Int = 2

@main def Test =
  val a = A()
  assert(a.foo == 1)
  val b = B()
  assert(b.foo == 2)
