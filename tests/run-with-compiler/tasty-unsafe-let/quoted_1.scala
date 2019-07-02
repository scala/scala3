import scala.quoted._

object Macros {

  inline def let[T](rhs: T)(body: => T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T](rhs: Expr[T], body: Expr[T => Unit]) given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val rhsTerm = rhs.unseal

    import qctx.tasty.util.{let => letTerm}
    letTerm(rhsTerm) { rhsId =>
      body(rhsId.seal.asInstanceOf[Expr[T]]).unseal // Dangerous uncheked cast!
    }.seal.cast[Unit]
  }


}
