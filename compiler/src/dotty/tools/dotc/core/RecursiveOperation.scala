package dotty.tools
package dotc
package core

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.reporting.Message

type RecursiveOperationDetails = Showable | (() => String)

/**
 * Operation that may cause unbounded recursion depending on user input.
 * @param title the operation title
 * @param details the lazily-initialized operation details
 * @param weight the operation weight, used to prioritize some operations when displaying error messages
 */
final class RecursiveOperation(var title: String, var details: RecursiveOperationDetails, var weight: Int):
  def explanation(using Context): String = details match
    case f: (() => ?) => s"$title ${f()}"
    case s: Showable => i"$title $s"

  def copy(): RecursiveOperation = RecursiveOperation(title, details, weight)

object RecursiveOperation:
  def blank(): RecursiveOperation = RecursiveOperation("", () => "", 0)

/**
 * Thrown when recursing too deep, as an alternative to triggering a stack overflow.
 *
 * @param rawOps the recursive operations, most recent last
 * rawOverflow* = The op that caused an overflow
 */
final class RecursionOverflow(private val rawOps: Array[RecursiveOperation],
                              private val rawOverflowTitle: String,
                              private val rawOverflowDetails: RecursiveOperationDetails,
                              private val rawOverflowWeight: Int)(using Context) extends TypeError:
  private val ops = RecursiveOperation(rawOverflowTitle, rawOverflowDetails, rawOverflowWeight) :: rawOps.map(_.copy()).reverse.toList

  override def fillInStackTrace(): Throwable =
    this

  private def opsString(rs: List[RecursiveOperation])(using Context): String = {
    val maxShown = 20
    if (rs.lengthCompare(maxShown) > 0)
      i"""${opsString(rs.take(maxShown / 2))}
         |  ...
         |${opsString(rs.takeRight(maxShown / 2))}"""
    else
      rs.map(_.explanation).mkString("\n  ", "\n|  ", "")
  }

  override def toMessage(using Context): Message =
    val mostCommon = ops.groupBy(_.title).toList.maxBy(_._2.map(_.weight).sum)._2
    em"""Recursion limit exceeded.
        |Maybe there is an illegal cyclic reference?
        |If that's not the case, you could also try to increase the stacksize using the -Xss JVM option.
        |For the unprocessed stack trace, compile with -Xno-enrich-error-messages.
        |A recurring operation is (inner to outer):
        |${opsString(mostCommon).stripMargin}"""
