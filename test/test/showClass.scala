package test

import dotty.tools.dotc.core.Decorators._

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
//      showClasses("dotty.tools.dotc.core.pickling.AbstractFileReader")
//      showPackage("scala.reflect")
//      showPackage("scala.collection")
    showPackage("dotty")
    println("done")
  }
}
