import scala.quoted.Expr
object Macro {
  import quoted.Liftable.{IntIsLiftable => _}
  rewrite def foo(transparent n: Int): Int = ~{
    '(n)
  }
}