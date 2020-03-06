import scala.tasty._
import scala.tasty.inspector._

@main def Test = {
  val inspector = new TastyInspector {
    def processCompilationUnit(reflect: Reflection)(tree: reflect.Tree): Unit = {
      import reflect.{_, given _}
      println(tree.show)
    }
  }
  inspector.inspect("", List("scala.tasty.Reflection"))
}
