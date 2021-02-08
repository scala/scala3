import scala.quoted.*

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(inline sc: StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes) : Expr[String] = {
    import q.reflect.*
    given Printer[Tree] = Printer.TreeStructure
    Expr(sc.asTerm.underlyingArgument.show + "\n" + args.asTerm.underlyingArgument.show)
  }
}
