import scala.quoted._

import scala.tasty.Reflection

object Macros {


  def impl(reflect: Reflection): Unit = {
    import reflect.{_, given}

    def foo(tree: Tree, term: Term, typeTree: TypeTree, parent: Tree) = {

      tree match {
        case tree: Tree =>
      }

      term match {
        case term: Term =>
      }

      typeTree match {
        case typeTree: TypeTree =>
      }

      parent match {
        case typeTree: Term =>
        case typeTree: TypeTree =>
      }

    }
  }

}
