import scala.quoted._

import scala.tasty._

object Foo {

  inline def inspectBody(i: => Int): String =
    ~inspectBodyImpl('(i))

  def inspectBodyImpl(x: Expr[Int])(implicit tasty: Tasty): Expr[String] = {
    import tasty._

    def definitionString(tree: Tree): Expr[String] = tree.symbol match {
      case IsClassSymbol(sym) => sym.tree.show.toExpr
      case IsDefSymbol(sym) => sym.tree.show.toExpr
      case IsValSymbol(sym) => sym.tree.show.toExpr
      case _ => '("NO DEFINTION")
    }

    x.reflect match {
      case Term.Inlined(None, Nil, arg) => definitionString(arg)
      case arg => definitionString(arg) // TODO should all by name parameters be in an inline node
    }
  }
}
