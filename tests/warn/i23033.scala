//> using options -Wunused:all

import scala.annotation.unused
import scala.concurrent.ExecutionContext
import scala.util.NotGiven

object Test {
  given [T](using @unused ev: NotGiven[T <:< Int]): AnyRef with {} // warn, NotGiven is a marker trait
}
object Useful:
  given [T](using @unused ec: ExecutionContext): AnyRef with {} // nowarn, ec is marked unused

object Syntax:
  given [T] => (@unused ec: ExecutionContext) => AnyRef // nowarn, ec is marked unused

class i23122(@unused param: AnyRef) // nowarn, param is marked unused
