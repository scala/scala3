import scala.quoted._

object Test {

  implicit def ListIsLiftableOr[T: Type, U: Type]: Liftable[List[T | U]] = new {
    def toExpr(xs: List[T | U]): Expr[List[T | U]] = '{ Nil: List[T | U] }
  }

  implicit def ListIsLiftableAnd[T: Type, U: Type]: Liftable[List[T & U]] = new {
    def toExpr(xs: List[T & U]): Expr[List[T & U]] = '{ Nil: List[T & U] }
  }

}
