//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
object G {
  def unapply(m: Any): Maybe[?, Unit] = Ok("")
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
