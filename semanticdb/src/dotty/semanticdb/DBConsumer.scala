package dotty.semanticdb

import scala.tasty.Tasty
import scala.tasty.file.TastyConsumer
import scala.tasty.util.TreeTraverser

class DBConsumer extends TastyConsumer {

  final def apply(tasty: Tasty)(root: tasty.Tree): Unit = {
    import tasty._
    object Traverser extends TreeTraverser[tasty.type](tasty) {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsDefinition(tree) =>
          println(tree.name)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(tasty.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
