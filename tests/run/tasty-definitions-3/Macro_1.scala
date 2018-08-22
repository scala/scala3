import scala.quoted._
import scala.tasty._

object Foo {

  transparent def inspectBody(i: => Int): String =
    ~inspectBodyImpl('(i))(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  def inspectBodyImpl(x: Expr[Int])(implicit tasty: Tasty): Expr[String] = {
    import tasty._
     def definitionString(tree: Tree): Expr[String] = tree.symbol.tree match {
      case Some(definition) => definition.show.toExpr
      case None => '("NO DEFINTION")
    }
    x.toTasty match {
      case Term.Inlined(None, Nil, arg) => definitionString(arg)
      case arg => definitionString(arg) // TODO should all by name parameters be in an inline node?
    }
  }
}
