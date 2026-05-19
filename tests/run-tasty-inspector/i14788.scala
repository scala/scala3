
import scala.quoted.*
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.reflect.ClassTag

import scala.quoted.*
import scala.tasty.inspector.*

@main def Test: Unit = {
  // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
  val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
  val tastyFiles = allTastyFiles.filter(_.contains("MySeq"))

  TastyInspector.inspectTastyFiles(tastyFiles)(new MyInspector)
}

class MySeq(override val length: Int) extends collection.Seq[String] {
  def foo: Int = length // error

  def apply(v1: Int): String = ???
  def iterator: Iterator[String] = ???
}

class MyInspector extends Inspector {
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect.*
    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        if (tree.isExpr) {
          try {
            tree.asExpr match {
              case '{ ($x: collection.Seq[t]).length } =>
                super.traverseTree(tree)(owner)
              case _ =>
                super.traverseTree(tree)(owner)
            }
          } catch {
            case e =>
              report.error(s"unexpected error ${e}", tree.pos)
              throw e
          }
        } else {
          super.traverseTree(tree)(owner)
        }
      }
    }
    tastys.foreach{ tasty =>
      traverser.traverseTree(tasty.ast)(tasty.ast.symbol)
    }
  }
}
