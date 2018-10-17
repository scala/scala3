import scala.quoted._

object Macros {


  def impl(staging: StagingContext): Unit = {
    import staging.reflection._

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
