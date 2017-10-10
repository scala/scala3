package dotty.tools.dotc
package core

import Contexts._
import config.Printers.typr

trait ConstraintRunInfo { self: RunInfo =>
  private[this] var maxSize = 0
  private[this] var maxConstraint: Constraint = _
  def recordConstraintSize(c: Constraint, size: Int) =
    if (size > maxSize) {
      maxSize = size
      maxConstraint = c
    }
  def printMaxConstraint()(implicit ctx: Context) =
    if (maxSize > 0) typr.println(s"max constraint = ${maxConstraint.show}")
}
