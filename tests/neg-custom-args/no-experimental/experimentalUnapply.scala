import scala.annotation.experimental

@experimental // FIXME ERROR
class A

object Extractor1:
  def unapply(s: Any): Option[A] = ??? // FIXME ERROR

object Extractor2:
  @experimental // FIXME ERROR
  def unapply(s: Any): Option[Int] = ???

def test: Unit =
  (??? : Any) match
    case _: A => // error // error
    case Extractor1(_) => // error
    case Extractor2(_) => // error
  ()
