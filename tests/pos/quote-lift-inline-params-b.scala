import scala.quoted.Expr
import quoted.Liftable.{IntIsLiftable => _}
object Macro {
  rewrite def foo(transparent n: Int): Int = ~{
    '(n)
  }
}