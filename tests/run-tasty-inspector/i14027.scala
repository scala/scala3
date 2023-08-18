import scala.quoted.*
import scala.tasty.inspector.*

class MyTest:
  List() match
    case Nil => 1
    case _ :: tail => 2

class TestInspector extends Inspector:
  override def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*
    val tree: Tree = tastys(0).ast
    val traverser = new TreeTraverser {}
    traverser.traverseTree(tree)(tree.symbol)

object Test {
  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("MyTest"))

    val inspect = new TestInspector()
    TastyInspector.inspectTastyFiles(allTastyFiles)(inspect)
  }
}