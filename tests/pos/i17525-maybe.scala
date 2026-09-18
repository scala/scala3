//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Extract {
  transparent inline def unapply(value: String): Tuple? = (1, "two")
}
def fail(): Unit = "" match { case Extract(a, b) => f(a, b) }
def f(n: Int, s: String): Unit = ()
