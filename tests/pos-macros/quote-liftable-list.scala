import scala.quoted.*

object Test {

  implicit def ListIsToExpr[T: ToExpr: Type]: ToExpr[List[T]] = new {
    def apply(xs: List[T])(using Quotes) = '{ Nil: List[T] }
  }

}
