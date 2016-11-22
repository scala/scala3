package dotty.tools

import dotc.core.Decorators._

object showClass extends ShowClassTests {

  def main(args: Array[String]) = {
    for (arg <- args) showPackage(ctx.requiredPackage(arg))
//      showClasses("test.SyncOps")
//      showClasses("scala.concurrent.forkjoin.LinkedTransferQueue")
//      showPackage("scala.reflect")
//      showPackage("scala.collection")

    showPackage("dotty", 1)
    showPackage("scala", 2)
  }
}
