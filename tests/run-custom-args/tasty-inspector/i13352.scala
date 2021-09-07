import scala.quoted.*
import scala.tasty.inspector.*

@main def Test = {
  // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
  val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
  val tastyFiles = allTastyFiles.filter(_.contains("CanEqual2"))

  TastyInspector.inspectTastyFiles(tastyFiles)(new MyInspector)
}

class MyInspector extends Inspector:

  override def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*
    class Traverser extends TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol) =
        if tree.pos.startLine < 100 then
          super.traverseTree(tree)(owner)
    end Traverser

    val traverser = new Traverser
    tastys.foreach { tasty =>
      traverser.traverseTree(tasty.ast)(tasty.ast.symbol)
    }


class CanEqual2[T]
