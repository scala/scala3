trait Expr[T]
trait Liftable[T]

object test1 {
  class ToExpr[T] given Liftable[T] extends Conversion[T, Expr[T]] {
    def apply(x: T): Expr[T] = ???
  }
  given toExpr[T] as ToExpr[T] given Liftable[T]

  given as Liftable[Int] = ???
  given as Liftable[String] = ???

  def x = the[ToExpr[String]]
  def y = the[Conversion[String, Expr[String]]]

  def a: Expr[String] = "abc"
}

object test2 {

  given autoToExpr[T] as Conversion[T, Expr[T]] given Liftable[T] {
    def apply(x: T): Expr[T] = ???
  }

  given as Liftable[Int] = ???
  given as Liftable[String] = ???

  def a: Expr[String] = "abc"
}