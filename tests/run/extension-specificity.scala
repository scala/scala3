class A
class B extends A

given a: (x: A) extended with
  def foo: Int = 1

given b: (x: B) extended with
  def foo: Int = 2

@main def Test =
  val a = A()
  assert(a.foo == 1)
  val b = B()
  assert(b.foo == 2)
