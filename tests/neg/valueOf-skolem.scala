case class Foo(
  aaaa: Int
)

case class Bar(
  foo: Foo,
  bar: Bla[foo.aaaa.type]
)

class Bla[T](using Ev[T])

class Ev[T](x: T)
object Ev:
  inline given ev: [T] => Ev[T] =
    Ev(valueOf[T])

object Test:
  def test: Unit =
    val x =
      Bar(
          Foo(0),
          Bla() // error
      )
