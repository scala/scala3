
import scala.quoted.*
import scala.tasty.inspector.*

@main def Test: Unit = {
  val classpath = System.getProperty("java.class.path").split(java.io.File.pathSeparatorChar).toList
  val jar = classOf[dotty.tools.dotc.Compiler].getProtectionDomain.getCodeSource.getLocation.getPath
  println(classpath)
  TastyInspector.inspectAllTastyFiles(
    tastyFiles = Nil,
    jars = jar :: Nil,
    dependenciesClasspath = classpath.filter(_.contains("scala3-compiler_3"))
  )(new MyInspector)
}


class MyInspector extends Inspector {
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
      import quotes.reflect.*
      val traverser = new TreeTraverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
          tree.pos.sourceCode
          super.traverseTree(tree)(owner)
        }
      }

      tastys.foreach { tasty =>
        if tasty.path == "dotty/tools/dotc/core/Phases.class" then
          val tree = tasty.ast
          traverser.traverseTree(tree)(tree.symbol)
      }
    }
}
