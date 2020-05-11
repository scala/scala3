package dotty.internal

import scala.quoted._

object CompileTimeMacros:
  def codeExpr(using qctx: QuoteContext)(sc: Expr[StringContext], args: Expr[Seq[Any]]): Expr[String] =
    throw new Exception("Non bootstrapped library")
