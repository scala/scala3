import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Foo[BooAny](any)
    foo(a.asInstanceOf[Foo[BooNothing]].x) // should not be possible
    foo(a.asInstanceOf[Foo[BooNothing]].y) // should not be possible

    val b = new Foo[BooNothing](a.asInstanceOf[Foo[BooNothing]].x)
    b.asInstanceOf[Foo[BooAny]].z(any) // should not be possible
  }

  def foo(x: BooNothing) = println("foo")

}

class Foo[T <: BooAny](val x: T) { // error
  def y: T = x
  def z(z: T) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
  def any: BooAny = assume
}
