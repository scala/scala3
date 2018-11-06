package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import scala.tasty.util.TreeTraverser

class SemanticdbConsumer extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser[reflect.type](reflect) {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsDefinition(tree) =>
          println(tree.name)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
