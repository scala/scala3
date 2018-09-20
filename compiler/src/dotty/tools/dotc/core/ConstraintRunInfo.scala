package dotty.tools.dotc
package core

import Contexts._
import config.Printers.{default, typr}

trait ConstraintRunInfo { self: Run =>
  private[this] var maxSize = 0
  private[this] var maxConstraint: Constraint = _
  def recordConstraintSize(c: Constraint, size: Int) =
    if (size > maxSize) {
      maxSize = size
      maxConstraint = c
    }
  def printMaxConstraint()(implicit ctx: Context) = {
    val printer = if (ctx.settings.YdetailedStats.value) default else typr
    if (maxSize > 0) printer.println(s"max constraint = ${maxConstraint.show}")
  }
  protected def reset() = maxConstraint = null
}
