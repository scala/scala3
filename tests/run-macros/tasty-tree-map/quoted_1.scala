import scala.quoted._

object Macros:
  implicit inline def identityMaped[T](x: => T): T = ${ MacrosImpl.impl('x) }

object MacrosImpl:
  def impl[T: Type](x: Expr[T])(using qctx: QuoteContext) : Expr[T] = {
    import qctx.reflect._
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).asExprOf[T]
    transformed
  }
