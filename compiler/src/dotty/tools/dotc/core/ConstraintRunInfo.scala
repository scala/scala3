package dotty.tools.dotc
package core

import Contexts._
import config.Printers.{default, typr}
import scala.compiletime.uninitialized

trait ConstraintRunInfo { self: Run =>
  private var maxSize = 0
  private var maxConstraint: Constraint | Null = uninitialized
  def recordConstraintSize(c: Constraint, size: Int): Unit =
    if (size > maxSize) {
      maxSize = size
      maxConstraint = c
    }
  def printMaxConstraint()(using Context): Unit =
    if maxSize > 0 then
      val printer = if ctx.settings.YdetailedStats.value then default else typr
      printer.println(s"max constraint size: $maxSize")
      try printer.println(s"max constraint = ${maxConstraint.nn.show}")
      catch case ex: StackOverflowError => printer.println("max constraint cannot be printed due to stack overflow")

  protected def reset(): Unit = maxConstraint = null
}
