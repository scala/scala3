package scala.tasty.interpreter

import scala.quoted._
import scala.tasty.inspector.TastyInspector

class TastyInterpreter extends TastyInspector {

  protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit = {
    import qctx.tasty._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, _, Some(rhs)) =>
          val interpreter = new jvm.Interpreter(this.reflect)

          interpreter.eval(rhs)(using Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)
      }
    }
    Traverser.traverseTree(root)
  }
}
