import scala.quoted._

object Macros {

  inline def let[T](rhs: => T)(inline body: T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T: Type](rhs: Expr[T], body: Expr[T => Unit])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val rhsTerm = rhs.asTerm

    import qctx.tasty.{let => letTerm}
    letTerm(rhsTerm) { rhsId =>
      Expr.betaReduce('{$body(${rhsId.asExpr.asInstanceOf[Expr[T]]})}).asTerm // Dangerous uncheked cast!
    }.asExprOf[Unit]
  }


}
