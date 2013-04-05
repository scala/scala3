package test

import dotty.tools.dotc.core.Decorators._

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
    showClasses("scala.reflect.runtime.SynchronizedOps")
//    showPackage("scala.reflect")
//    showPackage("scala")
    println("done")
  }
}
