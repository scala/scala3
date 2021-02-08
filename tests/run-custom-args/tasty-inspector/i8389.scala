import scala.quoted.*
import scala.tasty.inspector.*

@main def Test = {
  // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
  val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
  val tastyFiles = allTastyFiles.filter(_.contains("TraitParams"))

  // in dotty-example-project
  val inspector = new Inspector {
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
      for tasty <- tastys do
        println(tasty.ast.show)
    }
  }
  TastyInspector.inspectTastyFiles(tastyFiles)(inspector)
}

object TraitParams {

  trait Base(val msg: String)
  class A extends Base("Hello")
  class B extends Base("Dotty!")

  // Union types only exist in Dotty, so there's no chance that this will accidentally be compiled with Scala 2
  private def printMessages(msgs: (A | B)*) = println(msgs.map(_.msg).mkString(" "))

  def test: Unit = {

    printMessages(new A, new B)

    // Sanity check the classpath: this won't run if the dotty jar is not present.
    val x: Int => Int = z => z
    x(1)
  }
}
