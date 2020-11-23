
import scala.quoted._

object Macro {

  transparent inline def charOrString(inline str: String): Any = ${ impl('str) }

  def impl(strExpr: Expr[String]) (using Quotes)=
    val str = strExpr.unliftOrError
    if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
