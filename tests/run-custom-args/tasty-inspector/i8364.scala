import scala.quoted._
import scala.tasty.inspector._

@main def Test = {
  val inspector = new TastyInspector {
    protected def processCompilationUnit(using Quotes)(tree: quotes.reflect.Tree): Unit = {
      tree.showExtractors // Make sure that tree is loaded and can be traveresed
    }
  }

   // Artefact of the current test infrastructure
  // TODO improve infrastructure to avoid needing this code on each test
  val libJarClasspath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(x => x.contains("scala3-library-bootstrapped") && x.endsWith(".jar")).get

  inspector.inspectTastyFilesInJar(libJarClasspath)
}
