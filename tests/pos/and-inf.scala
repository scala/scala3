class A
class B

class Inv[T]
class Contra[-T]

class Test {
  def foo[T, S](x: T, y: S): Contra[Inv[T] & Inv[S]] = ???
  val a: A = new A
  val b: B = new B

  val x: Contra[Inv[A] & Inv[B]] = foo(a, b)
}
