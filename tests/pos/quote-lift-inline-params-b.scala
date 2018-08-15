import scala.quoted.Expr
import quoted.Liftable.{IntIsLiftable => _}
object Macro {
  transparent def foo(n: Int & Constant): Int = ~{
    '(n)
  }
}