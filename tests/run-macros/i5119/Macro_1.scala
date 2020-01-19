import scala.quoted._
import scala.quoted.autolift.{given _}

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(sc: => StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]]) with (qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty.{_, given _}
    (sc.unseal.underlyingArgument.showExtractors + "\n" + args.unseal.underlyingArgument.showExtractors)
  }
}
