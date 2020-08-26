import scala.quoted._
import scala.tasty.inspector._

@main def Test = {
  val inspector = new TastyInspector {
    protected def processCompilationUnit(using QuoteContext)(tree: qctx.tasty.Tree): Unit = {
      println(tree.show)
    }
  }
  inspector.inspect("", List("scala.tasty.Reflection"))
}
