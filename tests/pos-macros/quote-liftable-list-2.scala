import scala.quoted._

object Test {

  implicit def ListIsLiftableOr[T: Staged, U: Staged]: Liftable[List[T | U]] = new {
    def toExpr(xs: List[T | U]) = '{ Nil: List[T | U] }
  }

}
