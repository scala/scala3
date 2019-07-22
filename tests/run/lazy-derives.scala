import scala.deriving._

object Test extends App {
  case class Mono(i: Int) derives Foo, Bar
  case class Poly[T](t: T) derives Foo, Bar, Baz

  trait Quux[T]
  object Quux {
    given [T] as Quux[T] = new Quux[T] {}
  }

  trait Foo[T]
  object Foo {
    given as Foo[Int] {}
    def derived[T] given (m: Mirror.Of[T]): Foo[T] = new Foo[T] {}
  }

  trait Bar[T]
  object Bar {
    given as Bar[Int] {}
    def derived[T] given (m: Mirror.Of[T], o: Quux[T]): Bar[T] = new Bar[T] {}
  }

  trait Baz[F[_]]
  object Baz {
    def derived[F[_]] given (m: Mirror { type MirroredType = F }): Baz[F] = new Baz[F] {}
  }

  // Unfortunately these tests doesn't distinguish a lazy val
  // from a cached def
  assert(the[Foo[Mono]] eq the[Foo[Mono]])
  assert(the[Bar[Mono]] eq the[Bar[Mono]])
  assert(the[Baz[Poly]] eq the[Baz[Poly]])

  // These have term arguments so should be distinct
  assert(the[Foo[Poly[Int]]] ne the[Foo[Poly[Int]]])
  assert(the[Bar[Poly[Int]]] ne the[Bar[Poly[Int]]])
}
