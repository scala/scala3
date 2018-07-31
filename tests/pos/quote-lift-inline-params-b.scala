import scala.quoted.Expr
import quoted.Liftable.{IntIsLiftable => _}
object Macro {
  transparent def foo(transparent n: Int): Int = ~{
    '(n)
  }
}