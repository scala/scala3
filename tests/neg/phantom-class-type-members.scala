import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Bar()
    foo(a.asInstanceOf[Foo{type T = BooNothing}].y)  // error

    a match {
      case a: Foo{type T = BooNothing} => a.y // error
    }

    val b = new Baz
    b.asInstanceOf[Foo{type T = BooAny}].z(any)  // error

    b match {
      case b: Foo{type T = BooAny} => a.z(any) // error
    }
  }

  def foo(x: BooNothing) = println("foo")

}

abstract class Foo {
  type T <: BooAny
  def y: T
  def z(z: T): Unit
}

class Bar extends Foo {
  type T = BooAny
  def y: T = any
  def z(z: T) = ()
}

class Baz extends Foo {
  type T = BooNothing
  def y: T = nothing
  def z(z: T) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
  def any: BooAny = assume
  def nothing: BooNothing = assume
}
