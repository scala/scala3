import scala.quoted._

import scala.tasty.Tasty

object Macros {


  def impl(tasty: Tasty): Unit = {
    import tasty._

    def foo(tree: Tree, term: Term, typeTree: TypeTree, parent: Parent) = {

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
