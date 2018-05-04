import scala.quoted._

object Macros {
  inline def h(inline f: Int => String): String = ~ '(f(42))
}
