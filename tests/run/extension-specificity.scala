class A
class B extends A

extension (x: A)
  def foo: Int = 1

extension (x: B)
  def foo: Int = 2

@main def Test =
  val a = A()
  assert(a.foo == 1)
  val b = B()
  assert(b.foo == 2)
