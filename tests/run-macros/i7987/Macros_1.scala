import scala.quoted._
import scala.deriving._


object Macros {
  inline def m(): String = ${ macroImpl() }

  def macroImpl[T]()(using Quotes): Expr[String] = {
    Expr.summon[Mirror.Of[Some[Int]]] match
      case Some('{ $_ : t }) => Expr(Type.show[t])
  }
}
