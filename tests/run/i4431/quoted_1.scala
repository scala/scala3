import scala.quoted._

object Macros {
  rewrite def h(f: => Int => String): String = ~ '(f(42))
}
