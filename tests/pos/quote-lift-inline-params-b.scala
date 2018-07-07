import scala.quoted.Expr
object Macro {
  inline def foo(inline n: Int): Int = ~{
    import quoted.Liftable.{IntIsLiftable => _}
    '(n)
  }
}