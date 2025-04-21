//> using options -feature

import language.experimental.into
import Conversion.into

into trait T
class C(x: Int) extends T

object Test:
  given Conversion[Int, C] = C(_)

  def f(x: T) = ()
  def g(x: C) = ()
  f(1)      // ok
  g(1)      // warn
