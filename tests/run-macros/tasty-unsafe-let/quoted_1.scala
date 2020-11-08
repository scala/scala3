import scala.quoted._

object Macros {

  inline def let[T](rhs: => T)(inline body: T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T: Type](rhs: Expr[T], body: Expr[T => Unit])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    ValDef.let(rhs.asReflectTree) { rhsId =>
      Expr.betaReduce('{$body(${rhsId.asExpr.asInstanceOf[Expr[T]]})}).asReflectTree // Dangerous uncheked cast!
    }.asExprOf[Unit]
  }


}
