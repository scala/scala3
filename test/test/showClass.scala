package test

import dotty.tools.dotc.core.Decorators._

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
//    showClasses("scala.tools.jline.console.history.MemoryHistory")
    showPackage("scala")
    println("done")
  }
}
