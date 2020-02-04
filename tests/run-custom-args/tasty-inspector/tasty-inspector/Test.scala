import scala.tasty.Reflection
import scala.tasty.reflect.TreeTraverser
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {
    new DBInspector().inspect("", List("Foo"))
  }
}

class DBInspector extends TastyInspector {

  protected def processCompilationUnit(reflect0: Reflection)(root: reflect0.Tree): Unit = {
    import reflect0.{_, given _}
    object Traverser extends TreeTraverser {
      val reflect: reflect0.type = reflect0
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case tree: Definition =>
          println(tree.showExtractors)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect0.rootContext)
  }

}
