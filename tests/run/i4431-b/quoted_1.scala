import scala.quoted.{_, given}

object Macros {
  inline def h(f: => Int => String): String = f(42)
}
