import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Bar()
    foo(a.asInstanceOf[Foo{type T = BooNothing}].y)  // error

    val b = new Baz
    b.asInstanceOf[Foo{type T = BooAny}].z(any)  // error
  }

  def foo(x: BooNothing) = println("foo")

}

abstract class Foo {
  type T <: BooAny // error
  def y: T
  def z(z: T) = ()
}

class Bar {
  type T = BooAny // error
  def y: T = any
  def z(z: T) = ()
}

class Baz {
  type T = BooNothing // error
  def z(z: T) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
  def any: BooAny = assume
}
