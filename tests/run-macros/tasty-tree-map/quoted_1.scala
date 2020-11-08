import scala.quoted._

object Macros {

  implicit inline def identityMaped[T](x: => T): T = ${ impl('x) }

  def impl[T: Type](x: Expr[T])(using qctx: QuoteContext) : Expr[T] = {
    import qctx.reflect.{_, given} // FIXME: #8919
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).asExprOf[T]
    transformed
  }

}
