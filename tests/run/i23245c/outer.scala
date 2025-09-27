
trait T:
  def f = 42
  trait D:
    lazy val g = f

object C extends T
