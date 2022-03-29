import scala.quoted.*
import scala.tasty.inspector.*

@main def Test = {
  // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
  val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
  val tastyFiles = allTastyFiles.filter(_.contains("App"))

  // in dotty-example-project
  val inspector = new Inspector {
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
      import quotes.reflect.*
      val traverser = new TreeTraverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
          try {
            super.traverseTree(tree)(owner)
          } catch {
            case e =>
              report.error(s"unexpected error ${e}", tree.pos)
              throw e
          }
        }
      }
      tastys.foreach{ tasty =>
        traverser.traverseTree(tasty.ast)(tasty.ast.symbol)
      }
    }
  }
  TastyInspector.inspectTastyFiles(tastyFiles)(inspector)
}

object App {
  import scala.compiletime.*

  transparent inline def summonFirst0[T]: Any =
    inline erasedValue[T] match
      case _: (a *: b) => summonFrom {
        case instance: `a` => instance
        case _ => summonFirst0[b]
      }
}
