import scala.language.implicitConversions

given [T]: Conversion[String => T, String => Option[T]] = ???
// This one is irrelevant, shouldn't be included in error message
given irrelevant[T]: Conversion[String => T, String => Byte] = ???

def test() = {
  given foo: (String => Int) = _ => 42

  val fails = summon[String => Option[Int]] // error
}
