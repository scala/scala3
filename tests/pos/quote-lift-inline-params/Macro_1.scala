import scala.quoted.Expr
object Macro {
  import quoted.Liftable.{IntIsLiftable => _}
  transparent def foo(transparent n: Int): Int = ~{
    '(n)
  }
}