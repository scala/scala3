package test

import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
//      showClasses("test.SyncOps")
//      showClasses("scala.concurrent.forkjoin.LinkedTransferQueue")
//      showPackage("scala.reflect")
//      showPackage("scala.collection")

    showPackage("dotty")
    showPackage("scala")
    println(s"${Symbols.stubs.length} stubs")
    println(Symbols.stubs mkString " ")
  }
}
