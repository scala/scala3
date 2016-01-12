class A
object A {
  implicit def theA: A = new A
}
class Foo[T]
object Foo {
  implicit def theFoo: Foo[A] = new Foo[A]
}

object Test {
  def getFooA(implicit foo: Foo[A]) = foo
  def getA(implicit a: A) = a

  getFooA
  getA
}
