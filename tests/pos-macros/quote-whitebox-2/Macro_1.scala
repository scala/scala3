
import scala.quoted._

object Macro {

  transparent inline def charOrString(inline str: String): Any = ${ impl('str) }

  def impl(using s: Scope)(strExpr: s.Expr[String]): s.Expr[Any] =
    val str = strExpr.unliftOrError
    if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
