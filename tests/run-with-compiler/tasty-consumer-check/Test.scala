import scala.tasty.Reflection
import scala.tasty.file._

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("notscala.collection.immutable.List"), new DBConsumer)
  }
}

class DBConsumer extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsClassDef(tree) =>
          println(tree.symbol.methods)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

}
