//> using options -Yexplicit-nulls


import language.experimental.magic
import scala.annotation.experimental

@experimental
class A

object Extractor1:
  def unapply(s: Any): A? = ??? // error

object Extractor2:
  @experimental
  def unapply(s: Any): Int? = ???

def test: Unit =
  (??? : Any) match
    case _: A => // error // error
    case Extractor1(_) => // error
    case Extractor2(_) => // error
  ()
