import scala.quoted.*
import scala.tasty.inspector.*

object Test {
  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("Foo"))

    TastyInspector.inspectTastyFiles(tastyFiles)(new DBInspector())
  }
}

class DBInspector extends Inspector {

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect.*
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case tree: Definition =>
          println(tree.show(using Printer.TreeStructure))
          super.traverseTree(tree)(owner)
        case tree =>
          super.traverseTree(tree)(owner)
      }

    }
    for tasty <- tastys do
      Traverser.traverseTree(tasty.ast)
  }

}
