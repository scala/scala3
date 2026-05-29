package dotty.tools
package dotc
package core

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.util.{NoSourcePosition, SrcPos}

// If possible, we want to directly reference a Showable to avoid allocating a closure for every `handleRecursive` call
type RecursiveOperationDetails = Showable | (() => String)

/**
 * Operation that may cause unbounded recursion depending on user input.
 *
 * Mutable so that this object can be reused without allocating, since it is changed a lot.
 *
 * @param title the operation title
 * @param details the operation details
 * @param pos the operation position
 * @param weight the operation weight, used to prioritize some operations when displaying error messages
 */
final class RecursiveOperation(var title: String, var details: RecursiveOperationDetails, var pos: SrcPos | Null, var weight: Int):
  def explanation(using Context): String =
    try
      ctx.handleRecursive("displaying error for", () => title):
        details match
          case f: (() => ?) => s"$title ${f()}"
          case s: Showable => i"$title $s"
    catch
      case _: RecursionOverflow => "<not enough fuel to show details>"

  def copy(): RecursiveOperation = RecursiveOperation(title, details, pos, weight)

object RecursiveOperation:
  def blank(): RecursiveOperation = RecursiveOperation("", () => "", NoSourcePosition, 0)

/**
 * Thrown when recursing too deep, as an alternative to triggering a stack overflow.
 *
 * This is _not_ a TypeError because otherwise it would get caught early and there wouldn't be enough stack
 * to properly display which operations caused it.
 *
 * @param ops the recursive operations, most recent first
 */
final class RecursionOverflow(ops: List[RecursiveOperation])(using val ctx: Context) extends Error:
  // We aren't going to show the stack trace anyway so might as well save the perf cost of throwing
  override def fillInStackTrace(): Throwable =
    this

  private def opsString(rs: List[RecursiveOperation]): String = {
    val maxShown = 20
    if (rs.lengthCompare(maxShown) > 0)
      i"""${opsString(rs.take(maxShown / 2))}
         |  ...
         |${opsString(rs.takeRight(maxShown / 2))}"""
    else
      rs.map(_.explanation).mkString("\n  ", "\n|  ", "")
  }

  def pos: SrcPos =
    ops.collectFirst{ case op if op.pos != null && op.pos != NoSourcePosition => op.pos.nn }.getOrElse(NoSourcePosition)

  def toMessage: Message =
    val mostCommon = ops.groupBy(_.title).toList.maxBy(_._2.map(_.weight).sum)._2
    em"""Recursion limit exceeded.
        |Maybe there is an illegal cyclic reference?
        |If that's not the case, you could try to increase the fuel and stack size: https://docs.scala-lang.org/overviews/compiler-options/compiling-deeply-nested-code.html
        |For the unprocessed stack overflow trace, compile with -Xno-enrich-error-messages.
        |A recurring operation is (inner to outer):
        |${opsString(mostCommon).stripMargin}"""

object RecursionOverflow:
  /**
   * Creates a RecursionOverflow for the given operations + overflowing operation
   *
   * @param rawOps the recursive operations, most recent last
   * rawOverflow* = The op that caused an overflow
   */
  def apply(rawOps: Array[RecursiveOperation],
            rawOverflowTitle: String,
            rawOverflowDetails: RecursiveOperationDetails,
            rawOverflowPosition: SrcPos | Null,
            rawOverflowWeight: Int)(using Context): RecursionOverflow =
    val ops = RecursiveOperation(rawOverflowTitle, rawOverflowDetails, rawOverflowPosition, rawOverflowWeight) :: rawOps.map(_.copy()).reverse.toList
    new RecursionOverflow(ops)