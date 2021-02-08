import scala.quoted.*

object Macros {

  inline def let[T](rhs: => T)(inline body: T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T: Type](rhs: Expr[T], body: Expr[T => Unit])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val rhsTerm = rhs.asTerm

    import quotes.reflect.*
    ValDef.let(Symbol.spliceOwner, rhsTerm) { rhsId =>
      Expr.betaReduce('{$body(${rhsId.asExpr.asInstanceOf[Expr[T]]})}).asTerm // Dangerous uncheked cast!
    }.asExprOf[Unit]
  }


}
