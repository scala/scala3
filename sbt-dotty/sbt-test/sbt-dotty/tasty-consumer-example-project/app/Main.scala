package hello

import scala.tasty.Reflection
import scala.tasty.file._

object Main extends App {


  class Consumer extends TastyConsumer {
    final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
      import reflect._
      val tastyStr = root.show
      println(tastyStr)
    }
  }

  ConsumeTasty("", List("lib.Foo"), new Consumer)

}
