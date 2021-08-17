import scala.annotation.experimental

@experimental
class A

object Extractor1:
  def unapply(s: Any): Option[A] = ??? // error

object Extractor2:
  @experimental
  def unapply(s: Any): Option[Int] = ???

def test: Unit =
  (??? : Any) match
    case _: A => // error // error
    case Extractor1(_) => // error
    case Extractor2(_) => // error
  ()
