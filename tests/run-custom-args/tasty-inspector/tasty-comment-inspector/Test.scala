import scala.tasty.Reflection
import scala.tasty.reflect.TreeTraverser
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {
    new CommentInspector().inspect("", List("Foo"))
  }
}

class CommentInspector extends TastyInspector {

  def processCompilationUnit(reflect0: Reflection)(root: reflect0.Tree): Unit = {
    import reflect0.{_, given _}
    object Traverser extends TreeTraverser {
      val reflect: reflect0.type = reflect0
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case tree: Definition =>
          tree.symbol.comment match {
            case Some(com) => println(com.raw)
            case None => println()
          }
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect0.rootContext)
  }

}
