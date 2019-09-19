import scala.quoted._


object Foo {

  inline def inspectBody(i: => Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._

    def definitionString(tree: Tree): Expr[String] = tree.symbol match {
      case IsClassDefSymbol(sym) => Expr(sym.tree.showExtractors)
      case IsDefDefSymbol(sym) => Expr(sym.tree.showExtractors)
      case IsValDefSymbol(sym) => Expr(sym.tree.showExtractors)
      case _ => '{"NO DEFINTION"}
    }

    x.unseal match {
      case Inlined(None, Nil, arg) => definitionString(arg)
      case arg => definitionString(arg) // TODO should all by name parameters be in an inline node
    }
  }

  def foo: Int = 1 + 2
  val bar: Int = 2 + 3
}
