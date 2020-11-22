import scala.quoted._

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(inline sc: StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) : Expr[String] = {
    import quotes.reflect._
    Expr(Term.of(sc).underlyingArgument.showExtractors + "\n" + Term.of(args).underlyingArgument.showExtractors)
  }
}
