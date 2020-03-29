
import scala.quoted._

object Macro {

  transparent inline def charOrString(inline str: String): Char | String = ${ impl(str) }

  def impl(str: String) = if (str.length == 1) Expr(str.charAt(0)) else Expr(str)

}
