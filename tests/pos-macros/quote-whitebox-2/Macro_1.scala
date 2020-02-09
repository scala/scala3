
import scala.quoted._

object Macro {

  inline def charOrString(inline str: String) <: Any = ${ impl('str) }

  def impl(strExpr: Expr[String]) (using QuoteContext)=
    val str = strExpr.value
    if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
