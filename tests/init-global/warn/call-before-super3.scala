abstract class Foo[T](defaultValue: => T, arg1: Int = 1, arg2: Int = 2):
  def getValue: T = defaultValue

enum Baz:
  case E1, E2 // warn

object Baz extends Foo[Baz](Baz.E1, arg2 = 2) // warn

@main def test = println(Baz.getValue)