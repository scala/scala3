import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  inline def printComment[T](t: => T): Unit =
    ${ impl('t) }

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val tree = x.unseal
    tree.symbol.comment.map(_.raw) match {
      case Some(str) => '{ println(${str}) }
      case None => '{ println() }
    }
  }
}
