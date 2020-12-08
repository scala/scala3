import scala.quoted._

object Foo {

  inline def inspectBody(inline i: Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(using Quotes) : Expr[String] = {
    import quotes.reflect._
    Term.of(x) match {
      case Inlined(None, Nil, arg) => Expr(arg.symbol.tree.show(using Printer.TreeStructure))
      case arg => Expr(arg.symbol.tree.show(using Printer.TreeStructure)) // TODO should all by name parameters be in an inline node?
    }
  }
}
