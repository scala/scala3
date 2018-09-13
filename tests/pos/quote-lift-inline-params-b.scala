import scala.quoted.Expr
import quoted.Liftable.{IntIsLiftable => _}
object Macro {
  inline def foo(transparent n: Int): Int = ~{
    '(n)
  }
}