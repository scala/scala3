import scala.quoted._


object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(using s: Scope)(arg1: s.Expr[Any], arg2: s.Expr[Any]): s.Expr[String] =
    Expr(arg1.underlyingArgument.showExtractors + "\n" + arg2.underlyingArgument.showExtractors)

}
