package dotty.internal

import scala.quoted._

object CompileTimeMacros:
  def codeExpr(using qctx: QuoteContext)(sc: Expr[StringContext], args: Expr[Seq[Any]]): Expr[String] =
    (sc, args) match
      case (Expr.StringContext(Consts(parts)), Varargs(args2)) =>
        Expr(StringContext(parts: _*).s(args2.map(_.show): _*))
      case _ =>
        report.throwError("compiletime.code must be used as a string interpolator `code\"...\"`")
