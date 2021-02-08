import scala.quoted.*


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
      case ('{ StringContext(${Varargs(parts)}*) }, Varargs(args)) =>
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import quotes.reflect.*
            report.error(msg, parts(partIdx).asTerm.pos)
          }
        }
        fooCore(parts, args, reporter)
    }
  }

  def fooErrors(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[List[(Int, Int, Int, String)]] = {
    (sc, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}*) }, Varargs(args)) =>
        val errors = List.newBuilder[Expr[(Int, Int, Int, String)]]
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import quotes.reflect.*
            val pos = parts(partIdx).asTerm.pos
            errors += '{ Tuple4(${Expr(partIdx)}, ${Expr(pos.start)}, ${Expr(pos.end)}, ${Expr(msg)}) }
          }
        }
        fooCore(parts, args, reporter) // Discard result
        Expr.ofList(errors.result())
    }


  }


  private def fooCore(parts: Seq[Expr[String]], args: Seq[Expr[Any]], reporter: Reporter)(using Quotes): Expr[String] = {
    for ((part, idx) <- parts.zipWithIndex) {
      val v = part.valueOrError
      if (v.contains("#"))
        reporter.errorOnPart("Cannot use #", idx)
    }

    '{ StringContext(${Expr.ofList(parts)}*).s(${Expr.ofList(args)}*) }
  }


  trait Reporter {
    def errorOnPart(msg: String, partIdx: Int): Unit
  }


}
