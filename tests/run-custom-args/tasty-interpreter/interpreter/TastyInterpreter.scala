package scala.tasty.interpreter

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector

class TastyInterpreter extends TastyInspector {

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect.{_, given _}
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, _, Some(rhs)) =>
          val interpreter = new jvm.Interpreter(reflect)

          interpreter.eval(rhs).with(Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)
      }
    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }
}
