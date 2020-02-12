package hello

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector

object Main extends App {

  val inspector = new TastyInspector {
    protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = {
      import reflect.{given _, _}
      val tastyStr = root.show
      println(tastyStr)
    }
  }

  inspector.inspect("", List("lib.Foo"))

}
