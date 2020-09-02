import scala.quoted._
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {
    new DBInspector().inspect("", List("Foo"))
  }
}

class DBInspector extends TastyInspector {

  protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit = {
    import qctx.tasty._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(using Owner): Unit = tree match {
        case tree: Definition =>
          println(tree.showExtractors)
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)
  }

}
