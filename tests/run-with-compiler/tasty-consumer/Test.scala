import scala.tasty.Tasty
import scala.tasty.util.TreeTraverser
import scala.tasty.file._

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("Foo"), new DBConsumer)
  }
}

class DBConsumer extends TastyConsumer {

  final def apply(tasty: Tasty)(root: tasty.Tree): Unit = {
    import tasty._
    object Traverser extends TreeTraverser[tasty.type](tasty) {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsDefinition(tree) =>
          println(tree.show)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(tasty.rootContext)
  }

}
