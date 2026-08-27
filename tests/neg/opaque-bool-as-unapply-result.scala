object Internal:
  opaque type Foo <: Boolean = Boolean

import Internal.*

object Bar:
  def unapply(arg: Int): Foo = ???

@main def main =
  1 match
    case Bar() => // error
