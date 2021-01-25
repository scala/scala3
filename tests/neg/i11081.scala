enum Outer:
  case Foo(u: Unavailable) // error
  case Bar(u: DefinitelyNotAvailable) // error
object Outer:
  class Unavailable(i: Int)
  case class DefinitelyNotAvailable()
