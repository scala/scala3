package hello

import scala.quoted._
import scala.tasty.inspector.TastyInspector

object Main extends App {

  val inspector = new TastyInspector {
    protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit = {
      import qctx.tasty._
      val tastyStr = root.show
      println(tastyStr)
    }
  }

  inspector.inspect("", List("lib.Foo"))

}
