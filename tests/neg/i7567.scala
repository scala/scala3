class A
class B extends A
class C extends A

object Foo {
  private[this] class Bar[+T](var x: T) // error: covariant type T occurs in contravariant position in type T of value x_=
  def foo: B = {
    val barB: Bar[B] = new Bar(new B)
    val barA: Bar[A] = barB
    barA.x = new C
    barB.x
  }
}