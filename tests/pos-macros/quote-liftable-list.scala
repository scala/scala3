import scala.quoted._

object Test {

  implicit def ListIsLiftable[T](using s: Scope)(using s.Liftable[T], s.Type[T]): s.Liftable[List[T]] = new {
    def toExpr(xs: List[T]) = '{ Nil: List[T] }
  }

}
