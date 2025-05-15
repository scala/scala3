//> using options -Werror -Wunused:all

import scala.annotation.unused
import scala.concurrent.ExecutionContext
import scala.util.NotGiven

object Test {
  given [T](using @unused ev: NotGiven[T <:< Int]): AnyRef with {}
}
object Useful:
  given [T](using @unused ec: ExecutionContext): AnyRef with {}
object Syntax:
  given [T] => (@unused ec: ExecutionContext) => AnyRef

class i23122(@unused param: AnyRef)
