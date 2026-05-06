
trait T:
  def f = 42
  trait D:
    lazy val g = f

object C extends T

// D parent of Z is taken as PureInterface under separate compilation
// and thus doesn't get an outer.
