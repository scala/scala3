package test

import dotty.tools.dotc.core.Decorators._

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
    //showPackage("scala.reflect")
    showClasses("scala.collection.convert.Wrappers")
//    showPackage("scala")
    println("done")
  }
}
