import scala.quoted._
import scala.quoted.autolift.given

object Foo {

  inline def inspectBody(i: => Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    def definitionString(sym: Symbol): Expr[String] =
      if sym.isBind then sym.pattern.showExtractors else sym.tree.showExtractors
    x.unseal match {
      case Inlined(None, Nil, arg) => definitionString(arg.symbol)
      case arg => definitionString(arg.symbol) // TODO should all by name parameters be in an inline node?
    }
  }
}
