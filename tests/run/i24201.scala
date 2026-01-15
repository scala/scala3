abstract class Foo[T](defaultValue: => T, arg1: Int = 1, arg2: Int = 2):
  def getDefault: T = defaultValue
  def getArg1: Int = arg1
  def getArg2: Int = arg2

enum Baz:
  case E1, E2

object Baz extends Foo[Baz](Baz.E1, arg2 = 3)

@main def Test =
  println(Baz.getDefault)
  println(Baz.getArg1)
  println(Baz.getArg2)
