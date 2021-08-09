import scala.quoted.*

object Macro_1:
  inline def stringLiteral(inline s: String): String = ${showExpr('s)}
  def showExpr(s: Expr[?])(using Quotes): Expr[String] = Expr(s.show.toString)
