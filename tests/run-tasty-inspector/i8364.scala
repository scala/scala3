import scala.quoted.*
import scala.tasty.inspector.*

@main def Test = {
  val inspector = new Inspector {
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
      for tasty <- tastys do
        tasty.ast.show(using quotes.reflect.Printer.TreeStructure) // Make sure that tree is loaded and can be traveresed
    }
  }

   // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val libJarClasspath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(x => x.contains("scala-library-bootstrapped") && x.endsWith(".jar")).get

  TastyInspector.inspectTastyFilesInJar(libJarClasspath)(inspector)
}
