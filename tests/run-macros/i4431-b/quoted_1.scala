import scala.quoted._

object Macros {
  inline def h(f: => Int => String): String = f(42)
}
