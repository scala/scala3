//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

import compiletime.Maybe
object G {
  def unapply(m: Any): Maybe[Any, Unit] = Ok("")
}

object H {
  def unapplySeq(m: Any): Seq[?]? = null
}

object Test {
  (0: Any) match {
    case G(v) => v
    case H(v) => v
    case _ =>
  }
}
