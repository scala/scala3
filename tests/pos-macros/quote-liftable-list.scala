import scala.quoted._

object Test {

  implicit def ListIsLiftable[T: Liftable: Staged]: Liftable[List[T]] = new {
    def toExpr(xs: List[T]) = '{ Nil: List[T] }
  }

}
