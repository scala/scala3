package hello

import scala.tasty.Reflection
import scala.tasty.file._

object Main extends App {


  class Consumer extends TastyInspector {
    final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
      import reflect._
      val tastyStr = root.show
      println(tastyStr)
    }
  }

  InspectTasty("", List("lib.Foo"), new Consumer)

}
