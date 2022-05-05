package dotty.tools.dotc
package core

import Contexts._
import config.Printers.{default, typr}

trait ConstraintRunInfo { self: Run =>
  private var maxSize = 0
  private var maxConstraint: Constraint | Uninitialized = _
  def recordConstraintSize(c: Constraint, size: Int): Unit =
    if (size > maxSize) {
      maxSize = size
      maxConstraint = c
    }
  def printMaxConstraint()(using Context): Unit = {
    val printer = if (ctx.settings.YdetailedStats.value) default else typr
    if (maxSize > 0) printer.println(s"max constraint = ${maxConstraint.nn.show}")
  }
  protected def reset(): Unit = maxConstraint = null
}
