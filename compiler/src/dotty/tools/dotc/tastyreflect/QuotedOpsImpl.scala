package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer

trait QuotedOpsImpl extends scala.tasty.reflect.QuotedOps with TastyCoreImpl {

  def QuotedExprDeco[T](x: scala.quoted.Expr[T]): QuotedExprAPI = new QuotedExprAPI {
    def toTasty(implicit ctx: Context): Term = PickledQuotes.quotedExprToTree(x)
  }

  def QuotedTypeDeco[T](x: scala.quoted.Type[T]): QuotedTypeAPI = new QuotedTypeAPI {
    def toTasty(implicit ctx: Context): TypeTree = PickledQuotes.quotedTypeToTree(x)
  }

  def TermToQuoteDeco(term: Term): TermToQuotedAPI = new TermToQuotedAPI {

    def toExpr[T: scala.quoted.Type](implicit ctx: Context): scala.quoted.Expr[T] = {
      typecheck(ctx)
      new scala.quoted.Exprs.TastyTreeExpr(term).asInstanceOf[scala.quoted.Expr[T]]
    }

    private def typecheck[T: scala.quoted.Type](ctx: Contexts.Context): Unit = {
      implicit val ctx0: Contexts.FreshContext = ctx.fresh
      ctx0.setTyperState(ctx0.typerState.fresh())
      ctx0.typerState.setReporter(new Reporter {
        def doReport(m: MessageContainer)(implicit ctx: Contexts.Context): Unit = ()
      })
      val tp = QuotedTypeDeco(implicitly[scala.quoted.Type[T]]).toTasty
      ctx0.typer.typed(term, tp.tpe)
      if (ctx0.reporter.hasErrors) {
        val stack = new Exception().getStackTrace
        def filter(elem: StackTraceElement) =
          elem.getClassName.startsWith("dotty.tools.dotc.tasty.TastyImpl") ||
            !elem.getClassName.startsWith("dotty.tools.dotc")
        throw new scala.tasty.TastyTypecheckError(
          s"""Error during tasty reflection while typing term
             |term: ${term.show}
             |with expected type: ${tp.tpe.show}
             |
               |  ${stack.takeWhile(filter).mkString("\n  ")}
             """.stripMargin
        )
      }
    }
  }
}
