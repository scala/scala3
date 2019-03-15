
import scala.quoted._

object Macro {

  inline def charOrString(inline str: String) <: Any = ${ impl(str) }

  def impl(str: String) = if (str.length == 1) str.charAt(0).toExpr else str.toExpr

}
