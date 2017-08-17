package xsbt

import dotty.tools.dotc._
import core.Contexts._

class TestDriver extends Driver {
  override protected def sourcesRequired = false

  def getCompiler(args: Array[String], rootCtx: Context) = {
    val (fileNames, ctx) = setup(args, rootCtx)
    (newCompiler(ctx), ctx)
  }
}
