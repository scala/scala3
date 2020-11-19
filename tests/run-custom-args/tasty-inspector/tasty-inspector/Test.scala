import scala.quoted._
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("Foo"))

    new DBInspector().inspectTastyFiles(tastyFiles)
  }
}

class DBInspector extends TastyInspector {

  protected def processCompilationUnit(using QuoteContext)(root: qctx.reflect.Tree): Unit = {
    import qctx.reflect._
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
