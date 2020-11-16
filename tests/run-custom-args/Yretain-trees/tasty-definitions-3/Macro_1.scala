import scala.quoted._

object Foo {

  inline def inspectBody(inline i: Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.reflect._
    Term.of(x) match {
      case Inlined(None, Nil, arg) => Expr(arg.symbol.tree.showExtractors)
      case arg => Expr(arg.symbol.tree.showExtractors) // TODO should all by name parameters be in an inline node?
    }
  }
}
