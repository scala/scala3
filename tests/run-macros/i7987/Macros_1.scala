import scala.quoted._
import scala.deriving._
import scala.quoted.matching._

object Macros {
  inline def m(): String = ${ macroImpl() }

  def macroImpl[T]()(given qctx: QuoteContext): Expr[String] = {
    summonExpr[Mirror.Of[Some[Int]]] match
      case Some('{ $_ : $t }) => Expr(t.show)
  }
}
