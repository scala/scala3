import scala.quoted.Expr
import quoted.Liftable.{IntIsLiftable => _}
object Macro {
  inline def foo(inline n: Int): Int = ${
    'n
  }
}