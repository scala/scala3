
import scala.quoted._

object Macro {

  inline def charOrString(inline str: String) <: Any = ${ impl(str) }

  def impl(str: String)(given QuoteContext) = if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
