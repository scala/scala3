package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.quoted.PickledQuotes

trait QuotedOpsImpl extends scala.tasty.reflect.QuotedOps with CoreImpl {

  def QuotedExprDeco[T](x: scala.quoted.Expr[T]): QuotedExprAPI = new QuotedExprAPI {
    def unseal(implicit ctx: Context): Term = PickledQuotes.quotedExprToTree(x)
  }

  def QuotedTypeDeco[T](x: scala.quoted.Type[T]): QuotedTypeAPI = new QuotedTypeAPI {
    def unseal(implicit ctx: Context): TypeTree = PickledQuotes.quotedTypeToTree(x)
  }

  def TermToQuoteDeco(term: Term): TermToQuotedAPI = new TermToQuotedAPI {

    def seal[T: scala.quoted.Type](implicit ctx: Context): scala.quoted.Expr[T] = {
      typecheck()
      new scala.quoted.Exprs.TastyTreeExpr(term).asInstanceOf[scala.quoted.Expr[T]]
    }

    private def typecheck[T: scala.quoted.Type]()(implicit ctx: Context): Unit = {
      val tpt = QuotedTypeDeco(implicitly[scala.quoted.Type[T]]).unseal
      if (!(term.tpe <:< tpt.tpe)) {
        throw new scala.tasty.TastyTypecheckError(
          s"""Term: ${term.show}
             |did not conform to type: ${tpt.tpe.show}
             |""".stripMargin
        )
      }
    }
  }
}
