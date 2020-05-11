package dotty.internal

import scala.quoted._

object CompileTimeMacros:
  def codeExpr(using s: Scope)(sc: s.Expr[StringContext], args: s.Expr[Seq[Any]]): s.Expr[String] =
    (sc, args) match
      case (Expr.StringContext(Consts(parts)), Varargs(args2)) =>
        Expr(StringContext(parts: _*).s(args2.map(_.show): _*))
      case _ =>
        report.throwError("compiletime.code must be used as a string interpolator `code\"...\"`")
