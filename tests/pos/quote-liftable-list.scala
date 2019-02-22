import scala.quoted._

object Test {

  implicit def ListIsLiftable[T: Liftable: Type]: Liftable[List[T]] = new {
    def toExpr(xs: List[T]): Expr[List[T]] = '{ Nil: List[T] }
  }

}
