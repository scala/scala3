import scala.quoted._

object Macros {

  implicit inline def identityMaped[T](x: => T): T = ${ impl('x) }

  def impl[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[T] = {
    import s.tasty.{_, given _} // FIXME: #8919
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x).seal.cast[T]
    transformed
  }

}
