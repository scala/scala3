import scala.tasty.Reflection
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {
    new DBInspector().inspect("", List("Foo"))
  }
}

class DBInspector extends TastyInspector {

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect.{_, given _}
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case tree: Definition =>
          println(tree.showExtractors)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

}
