import scala.quoted.Expr
object Macro {
  transparent def foo(transparent n: Int): Int = ~{
    import quoted.Liftable.{IntIsLiftable => _}
    '(n)
  }
}