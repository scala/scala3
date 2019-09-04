import scala.quoted._

object Macros {

  implicit inline def identityMaped[T](x: => T): T = ${ impl('x) }

  def impl[T: Type](x: Expr[T]) given (qctx: QuoteContext): Expr[T] = {
    import qctx.tasty._
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).seal.cast[T]
    transformed
  }

}
