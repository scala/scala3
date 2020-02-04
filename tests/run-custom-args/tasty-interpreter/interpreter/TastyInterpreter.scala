package scala.tasty.interpreter

import scala.tasty.Reflection
import scala.tasty.reflect.TreeTraverser
import scala.tasty.inspector.TastyInspector

class TastyInterpreter extends TastyInspector {

  protected def processCompilationUnit(reflect0: Reflection)(root: reflect0.Tree): Unit = {
    import reflect0.{_, given _}
    object Traverser extends TreeTraverser {
      val reflect: reflect0.type = reflect0
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, _, Some(rhs)) =>
          val interpreter = new jvm.Interpreter(reflect)

          interpreter.eval(rhs)(using Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)
      }
    }
    Traverser.traverseTree(root)(reflect0.rootContext)
  }
}
