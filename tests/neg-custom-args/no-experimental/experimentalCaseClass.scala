import scala.annotation.experimental

@experimental
case class Foo(a: Int)

@experimental
case class Bar(a: Int)

object Bar:
  def f(): Unit = ()

def test: Unit =
  Foo(2) // error
  val x: Foo = ??? // error

  x match
    case Foo(a) => // error


  Bar(2) // error
  val y: Bar = ??? // error

  y match
    case Bar(a) => // error

  Bar.f() // error
