
import scala.quoted.*

object Macro {

  transparent inline def charOrString(inline str: String): Any = ${ impl('str) }

  def impl(strExpr: Expr[String]) (using Quotes)=
    val str = strExpr.valueOrError
    if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
