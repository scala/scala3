trait Magic[F]:
  extension (x: Int) def read: F

object Magic:
  given Magic[String]:
    extension(x: Int) def read: String =
      println("In string")
      s"$x"

opaque type Foo = String
object Foo:
  import Magic.given
  def apply(s: String): Foo = s

  given Magic[Foo]:
    extension (x: Int) def read: Foo =
      println("In foo")
      Foo(s"$x")

  def test: Unit =
    (3.read: Foo)  // error: ambiguous


@main def Test = {
  Foo.test
}