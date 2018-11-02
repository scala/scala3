package xsbt

import dotty.tools.dotc._
import core.Contexts._

class TestDriver extends Driver {
  override protected def sourcesRequired = false

  def getCompiler(args: Array[String], rootCtx: ContextRenamed) = {
    val (fileNames, ctx) = setup(args, rootCtx)
    (newCompiler(ctx), ctx)
  }
}
