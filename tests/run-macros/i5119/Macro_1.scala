import scala.quoted._
import scala.tasty.Reflection
import scala.quoted.autolift._

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(sc: => StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]]) given (qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    (sc.unseal.underlyingArgument.showExtractors + "\n" + args.unseal.underlyingArgument.showExtractors)
  }
}
