import scala.tasty.Reflection
import scala.tasty.inspector._

object Test {
  def main(args: Array[String]): Unit = {

    // Tasty Scala Class
    val inspector = new TestInspector_NonTasty()
    inspector.inspect("", List("scala.Option"))
    assert(inspector.isAlreadyLoaded)
    assert(inspector.className == "scala.Option")
  }
}

class TestInspector_NonTasty() extends TastyInspector:

  var isAlreadyLoaded: Boolean = false
  var className: String = ""

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    isAlreadyLoaded = reflect.Source.isAlreadyLoadedCompilationUnit
    className = reflect.Source.compilationUnitClassname
