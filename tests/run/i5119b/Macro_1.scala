import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(arg1: Expr[Any], arg2: Expr[Any])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    (arg1.unseal.underlyingArgument.showExtractors + "\n" + arg2.unseal.underlyingArgument.showExtractors)
  }

}
