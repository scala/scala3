import scala.quoted._

import scala.tasty.Reflection

object Macros {


  def impl(reflect: Reflection): Unit = {
    import reflect._

    def foo(tree: Tree, term: Term, typeTree: TypeTree, parent: Tree) = {

      tree match {
        case IsTerm(tree) =>
      }

      term match {
        case IsTerm(term) =>
      }

      typeTree match {
        case IsTypeTree(typeTree) =>
      }

      parent match {
        case IsTerm(typeTree) =>
        case IsTypeTree(typeTree) =>
      }

    }
  }

}
