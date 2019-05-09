import scala.quoted.Expr
object Macro {
  inline def foo(inline n: Int): Int = ${
    'n
  }
}