import scala.quoted._

import scala.tasty._

object Macros {

  inline def let[T](rhs: T)(body: => T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T](rhs: Expr[T], body: Expr[T => Unit])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val rhsTerm = rhs.unseal

    import reflect.util.{let => letTerm}
    letTerm(rhsTerm) { rhsId =>
      body(rhsId.seal.asInstanceOf[Expr[T]]).unseal // Dangerous uncheked cast!
    }.seal.cast[Unit]
  }


}
