import scala.quoted._


object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(arg1: Expr[Any], arg2: Expr[Any])(using q: Quotes) : Expr[String] =
    import q.reflect._
    given Printer[Tree] = Printer.TreeStructure
    Expr(Term.of(arg1).underlyingArgument.show + "\n" + Term.of(arg2).underlyingArgument.show)

}
