enum Outer with
  case Foo(u: Unavailable) // error
  case Bar(u: DefinitelyNotAvailable) // error
object Outer with
  class Unavailable(i: Int)
  case class DefinitelyNotAvailable()
