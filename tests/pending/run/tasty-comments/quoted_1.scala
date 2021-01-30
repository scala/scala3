import scala.quoted.*


object Macros {

  inline def printComment[T](t: => T): Unit =
    ${ impl('t) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val tree = x.asTerm
    tree.symbol.comment.map(_.raw) match {
      case Some(str) => '{ println(${str}) }
      case None => '{ println() }
    }
  }
}
