import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Foo[BooAny](any)
    foo(a.asInstanceOf[Foo[BooNothing]].x) // error
    foo(a.asInstanceOf[Foo[BooNothing]].y) // error

    a match {
      case a: Foo[BooNothing] => a.x // error
    }

    val b = new Foo[BooNothing](a.asInstanceOf[Foo[BooNothing]].x) // error
    b.asInstanceOf[Foo[BooAny]].z(any) // error

    b match {
      case b: Foo[BooAny] => b.z(any) // error
    }
  }

  def foo(x: BooNothing) = println("foo")

}

class Foo[T <: BooAny](val x: T) {
  def y: T = x
  def z(z: T) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
  def any: BooAny = assume
}
