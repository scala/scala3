import scala.quoted.*


object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ${ Macro.impl('{arg1}, '{arg2}) }

  def impl(arg1: Expr[Any], arg2: Expr[Any])(using q: Quotes) : Expr[String] =
    import q.reflect.*
    given Printer[Tree] = Printer.TreeStructure
    Expr(arg1.asTerm.underlyingArgument.show + "\n" + arg2.asTerm.underlyingArgument.show)

}
