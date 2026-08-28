//> using options -Yexplicit-nulls
import language.experimental.magic
import language.future
import scala.magic.*

def dbleErrTest[T <: Matchable](x: T?) =
  maybe:
    x? match
      case Err(e) =>
        val z = Ok(x)
        z

def x = dbleErrTest("one")
val _: Int = x // error
