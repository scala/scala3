package dotty.tools.dotc.core.tasty.experimental

import dotty.tools.tasty.experimental.Tasty

object DottyTasty extends Tasty with
  final val internal: DottyKernel.type = DottyKernel
