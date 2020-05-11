import scala.quoted._

object Test {

  implicit def ListIsLiftableAnd[T, U](using s: Scope)(using s.Type[T], s.Type[U]): s.Liftable[List[T & U]] = new {
    def toExpr(xs: List[T & U]) = '{ Nil: List[T & U] }
  }

}
