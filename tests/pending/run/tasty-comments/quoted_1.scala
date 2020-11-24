import scala.quoted._


object Macros {

  inline def printComment[T](t: => T): Unit =
    ${ impl('t) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val tree = Term.of(x)
    tree.symbol.comment.map(_.raw) match {
      case Some(str) => '{ println(${str}) }
      case None => '{ println() }
    }
  }
}
