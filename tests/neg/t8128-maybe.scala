//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
import compiletime.Maybe
object G {
  def unapply(m: Any): Maybe[?, Unit] = Ok("")  // error
}

