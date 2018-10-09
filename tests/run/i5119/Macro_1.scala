import scala.quoted._
import scala.tasty.Tasty

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ~Macro.impl('(sc), '(args))
  }
  implicit inline def XmlQuote(sc: => StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(implicit tasty: Tasty): Expr[String] = {
    import tasty._
    (sc.toTasty.underlyingArgument.show + "\n" + args.toTasty.underlyingArgument.show).toExpr
  }
}
