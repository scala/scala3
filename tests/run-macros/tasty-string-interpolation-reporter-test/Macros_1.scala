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

  def foo(using s: Scope)(sc: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[String] = {
    (sc, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import s.tasty._
            error(msg, parts(partIdx).pos)
          }
        }
        fooCore(parts, args, reporter)
    }
  }

  def fooErrors(using s: Scope)(sc: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[List[(Int, Int, Int, String)]] = {
    (sc, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val errors = List.newBuilder[s.Expr[(Int, Int, Int, String)]]
        val reporter = new Reporter {
          def errorOnPart(msg: String, partIdx: Int): Unit = {
            import s.tasty._
            val pos = parts(partIdx).pos
            errors += '{ Tuple4(${Expr(partIdx)}, ${Expr(pos.start)}, ${Expr(pos.end)}, ${Expr(msg)}) }
          }
        }
        fooCore(parts, args, reporter) // Discard result
        Expr.ofList(errors.result())
    }


  }


  private def fooCore(using s: Scope)(parts: Seq[s.Expr[String]], args: Seq[s.Expr[Any]], reporter: Reporter): s.Expr[String] = {
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
