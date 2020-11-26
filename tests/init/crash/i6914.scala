trait Expr[T]
trait Liftable[T]

object test1 {
  class ToExpr[T](using Liftable[T]) extends Conversion[T, Expr[T]] {
    def apply(x: T): Expr[T] = ???
  }
  given [T] => (Liftable[T]) => ToExpr[T] as toExprFun

  given Liftable[Int] = ???
  given Liftable[String] = ???

  def x = summon[ToExpr[String]]
  def y = summon[Conversion[String, Expr[String]]]

  def a: Expr[String] = "abc"
}

object test2 {

  given [T] => (Liftable[T]) => Conversion[T, Expr[T]] as autoToExpr {
    def apply(x: T): Expr[T] = ???
  }

  given Liftable[Int] = ???
  given Liftable[String] = ???

  def a: Expr[String] = "abc"
}