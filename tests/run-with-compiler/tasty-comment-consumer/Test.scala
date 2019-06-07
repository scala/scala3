import scala.tasty.Reflection
import scala.tasty.file._

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("Foo"), new CommentConsumer)
  }
}

class CommentConsumer extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsDefinition(tree) =>
          tree.symbol.comment match {
            case Some(com) => println(com.raw)
            case None => println()
          }
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

}
