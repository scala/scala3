import scala.quoted._
import scala.quoted.autolift.given

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(sc: => StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    (sc.unseal.underlyingArgument.showExtractors + "\n" + args.unseal.underlyingArgument.showExtractors)
  }
}
