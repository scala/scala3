import scala.quoted._

object Test {

  implicit def ListIsLiftableAnd[T: TypeTag, U: TypeTag]: Liftable[List[T & U]] = new {
    def toExpr(xs: List[T & U]) = '{ Nil: List[T & U] }
  }

}
