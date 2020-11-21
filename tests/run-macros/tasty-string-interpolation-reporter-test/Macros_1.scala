import scala.quoted._


import scala.language.implicitConversions

object Foo {
  implicit object StringContextOps {
    extension (inline ctx: StringContext) inline def foo (inline args: Any*): String = ${ Macro.foo('ctx, 'args) }
  }
}


object TestFooErrors { // Defined in tests
  implicit object StringContextOps {
    extension (inline ctx: StringContext) inline def foo (inline args: Any*): List[(Int, Int, Int, String)] = ${ Macro.fooErrors('ctx, 'args) }
  }
}

object Macro {

  def foo(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[String] = {
    (sc, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import qctx.reflect._
            Reporting.error(msg, Term.of(parts(partIdx)).pos)
          }
        }
        fooCore(parts, args, reporter)
    }
  }

  def fooErrors(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[List[(Int, Int, Int, String)]] = {
    (sc, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val errors = List.newBuilder[Expr[(Int, Int, Int, String)]]
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import qctx.reflect._
            val pos = Term.of(parts(partIdx)).pos
            errors += '{ Tuple4(${Expr(partIdx)}, ${Expr(pos.start)}, ${Expr(pos.end)}, ${Expr(msg)}) }
          }
        }
        fooCore(parts, args, reporter) // Discard result
        Expr.ofList(errors.result())
    }


  }


  private def fooCore(parts: Seq[Expr[String]], args: Seq[Expr[Any]], reporter: Reporter)(using Quotes): Expr[String] = {
    for ((part, idx) <- parts.zipWithIndex) {
      val Const(v: String) = part
      if (v.contains("#"))
        reporter.errorOnPart("Cannot use #", idx)
    }

    '{ StringContext(${Expr.ofList(parts)}: _*).s(${Expr.ofList(args)}: _*) }
  }


  trait Reporter {
    def errorOnPart(msg: String, partIdx: Int): Unit
  }


}
