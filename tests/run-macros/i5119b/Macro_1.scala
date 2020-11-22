import scala.quoted._


object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(arg1: Expr[Any], arg2: Expr[Any])(using Quotes) : Expr[String] =
    import quotes.reflect._
    Expr(Term.of(arg1).underlyingArgument.showExtractors + "\n" + Term.of(arg2).underlyingArgument.showExtractors)

}
