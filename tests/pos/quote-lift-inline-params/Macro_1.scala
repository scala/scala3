import scala.quoted.Expr
object Macro {
  import quoted.Liftable.{IntIsLiftable => _}
  inline def foo(inline n: Int): Int = ${
    'n
  }
}