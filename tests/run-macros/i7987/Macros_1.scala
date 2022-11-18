import scala.quoted.*
import scala.deriving.*


object Macros {
  transparent inline def m(): String = ${ macroImpl() } : String

  def macroImpl[T]()(using Quotes): Expr[String] = {
    Expr.summon[Mirror.Of[Some[Int]]] match
      case Some('{ $_ : t }) => Expr(Type.show[t])
  }
}
