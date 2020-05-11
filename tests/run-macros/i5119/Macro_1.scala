import scala.quoted._

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(inline sc: StringContext): StringContextOps = new StringContextOps(sc)
  def impl(using s: Scope)(sc: s.Expr[StringContext], args: s.Expr[Seq[Any]]): s.Expr[String] = {
    import s.tasty._
    Expr(sc.underlyingArgument.showExtractors + "\n" + args.underlyingArgument.showExtractors)
  }
}
