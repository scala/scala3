//> using options -Werror -deprecation -feature

import scala.quoted.*

object Macros {


  def impl(using Quotes): Unit = {
    import quotes.reflect.*

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
