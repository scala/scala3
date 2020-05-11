import scala.quoted._

object Macros {

  inline def let[T](rhs: => T)(inline body: T => Unit): Unit =
    ${ impl('rhs, 'body) }

  private def impl[T](using s: Scope)(rhs: s.Expr[T], body: s.Expr[T => Unit])(using s.Type[T]): s.Expr[Unit] = {
    s.tasty.let(rhs) { rhsId =>
      Expr.betaReduce('{$body(${rhsId.seal.asInstanceOf[s.Expr[T]]})}) // Dangerous uncheked cast!
    }.seal.cast[Unit]
  }

}
