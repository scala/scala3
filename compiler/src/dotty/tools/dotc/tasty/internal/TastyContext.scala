package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context

class TastyContext(val ctx: Context) extends scala.tasty.Context {
  override def toolbox: scala.runtime.tasty.Toolbox = Toolbox
}
