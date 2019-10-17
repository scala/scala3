import scala.quoted._

object Foo {

  inline def inspectBody(i: => Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}

    def definitionString(sym: Symbol): Expr[String] =
      if sym.isClassDef || sym.isDefDef || sym.isValDef then Expr(sym.tree.showExtractors)
      else '{"NO DEFINTION"}

    x.unseal match {
      case Inlined(None, Nil, arg) => definitionString(arg.symbol)
      case arg => definitionString(arg.symbol) // TODO should all by name parameters be in an inline node
    }
  }
}
