import scala.tasty.file._
import scala.tasty.interpreter.Interpreter
import scala.tasty.Reflection

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("IntepretedMain", "InterpretedBar"), new TastyInterpreter)
  }
}

class TastyInterpreter extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, _, Some(rhs)) =>
          val interpreter = new Interpreter(reflect)

          interpreter.eval(rhs)(Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)
      }
    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }
}
