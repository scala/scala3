import scala.quoted._


object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(arg1: Expr[Any], arg2: Expr[Any])(using qctx: QuoteContext) : Expr[String] =
    Expr(arg1.asReflectTree.underlyingArgument.showExtractors + "\n" + arg2.asReflectTree.underlyingArgument.showExtractors)

}
