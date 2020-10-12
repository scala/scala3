import scala.quoted._
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

  protected def processCompilationUnit(using QuoteContext)(root: qctx.reflect.Tree): Unit =
    isAlreadyLoaded = qctx.reflect.Source.isAlreadyLoadedCompilationUnit
    className = qctx.reflect.Source.compilationUnitClassname
