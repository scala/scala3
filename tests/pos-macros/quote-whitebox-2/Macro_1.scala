
import scala.quoted.{_, given}

object Macro {

  inline def charOrString(inline str: String) <: Any = ${ impl(str) }

  def impl(str: String)(given QuoteContext) = if (str.length == 1) str.charAt(0).toExpr else str.toExpr

}
