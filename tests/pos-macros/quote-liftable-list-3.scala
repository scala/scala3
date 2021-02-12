import scala.quoted.*

object Test {

  implicit def ListToExprAnd[T: Type, U: Type]: ToExpr[List[T & U]] = new {
    def apply(xs: List[T & U])(using Quotes) = '{ Nil: List[T & U] }
  }

}
