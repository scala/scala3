import scala.quoted._

object Macros {
  transparent def h(f: => Int => String): String = ~ '(f(42))
}
