import scala.quoted.Expr
object Macro {
  import quoted.Liftable.{IntIsLiftable => _}
  transparent def foo(n: Int & Constant): Int = ~{
    '(n)
  }
}
