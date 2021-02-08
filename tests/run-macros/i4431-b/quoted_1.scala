import scala.quoted.*

object Macros {
  inline def h(f: => Int => String): String = f(42)
}
