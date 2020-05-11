import scala.quoted._


object Macros {

  inline def printComment[T](t: => T): Unit =
    ${ impl('t) }

  def impl[T](using s: Scope)(x: s.Expr[T]): s.Expr[Unit] = {
    import s.tasty._

    val tree = x
    tree.symbol.comment.map(_.raw) match {
      case Some(str) => '{ println(${str}) }
      case None => '{ println() }
    }
  }
}
