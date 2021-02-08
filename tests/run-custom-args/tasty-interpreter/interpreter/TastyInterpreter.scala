package scala.tasty.interpreter

import scala.quoted.*
import scala.tasty.inspector.*

class TastyInterpreter extends Inspector {

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect.*

    object Traverser extends TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, Some(rhs)) =>
          val interpreter = new jvm.Interpreter

          interpreter.eval(rhs)(using Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)(owner)
      }
    }

    for tasty <- tastys do
      Traverser.traverseTree(tasty.ast)(Symbol.spliceOwner)
  }

}
