//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}
import compiletime.Maybe
object G {
  def unapply(m: Any): Maybe[?, Unit] = Ok("")  // error
}

