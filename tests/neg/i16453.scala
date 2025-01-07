import scala.language.implicitConversions

trait Foo { type T }

// This one is irrelevant, shouldn't be included in error message
given irrelevant: Long = ???

/** Use Scala 3 givens/conversions */
def testScala3() = {
  given c1[T]: Conversion[T, Option[T]] = ???
  given c2[F <: Foo](using f: F): Conversion[f.T, Option[f.T]] = ???
  given Conversion[Char, String] = ???
  given Conversion[Char, Option[Int]] = ???

  given foo: Foo:
    type T = Int
  given bar3: Int = 0
  given baz3: Char = 'a'

  // This should get the usual error
  summon[List[Int]] // error

  summon[Option[Int]] // error
  implicitly[Option[Char]] // error
  implicitly[String] // error
}

/** Use Scala 2 implicits */
def testScala2() = {
  implicit def toOpt[T](t: T): Option[T] = ???
  implicit def char2Str(c: Char): String = ???
  implicit val bar2: Int = 1
  implicit val baz2: Char = 'b'

  summon[String] // error
  implicitly[Option[Int]] // error
}
