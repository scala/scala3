import scala.language.implicitConversions

trait Foo { type T }

// Scala 3 style conversion
given [T]: Conversion[T, Option[T]] = ???
given [F <: Foo](using f: F): Conversion[f.T, Option[f.T]] = ???
// Scala 2 style conversion
implicit def toOption[T](t: T): Option[T] = Option(t)

// This one is irrelevant, shouldn't be included in error message
given irrelevant: Conversion[Int, Option[Long]] = ???

def test() = {
  given foo: Foo with
    type T = Int
  given bar: Int = 0

  summon[Option[Int]] // error
  implicitly[Option[Int]] // error
}
