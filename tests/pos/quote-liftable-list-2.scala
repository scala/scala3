import scala.quoted._

object Test {

  implicit def ListIsLiftableOr[T: TypeTag, U: TypeTag]: Liftable[List[T | U]] = new {
    def toExpr(xs: List[T | U]) = '{ Nil: List[T | U] }
  }

}
