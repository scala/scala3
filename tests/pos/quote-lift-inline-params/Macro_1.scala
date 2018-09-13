import scala.quoted.Expr
object Macro {
  import quoted.Liftable.{IntIsLiftable => _}
  inline def foo(transparent n: Int): Int = ~{
    '(n)
  }
}