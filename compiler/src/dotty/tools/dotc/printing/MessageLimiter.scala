package dotty.tools
package dotc
package printing

import core._
import Contexts._
import util.Property
import Texts.Text

abstract class MessageLimiter:

  protected def recurseLimit = 100
  protected var recurseCount: Int = 0

  def register(str: String): Unit = ()

  protected def recursionLimitExceeded()(using Context): Unit = ()

  final def controlled(op: => Text)(using Context): Text =
    if recurseCount < recurseLimit then
      try
        recurseCount += 1
        op
      finally
        recurseCount -= 1
    else
      recursionLimitExceeded()
      "..."

object MessageLimiter extends Property.Key[MessageLimiter]

class DefaultMessageLimiter extends MessageLimiter:
  override def recursionLimitExceeded()(using Context): Unit =
    if ctx.debug then
      report.warning("Exceeded recursion depth attempting to print.")
      Thread.dumpStack()

class SummarizeMessageLimiter(depth: Int) extends MessageLimiter:
  override val recurseLimit = recurseCount + depth
  override def recursionLimitExceeded()(using Context): Unit = ()

class ErrorMessageLimiter extends MessageLimiter:
  private val initialRecurseLimit = 50
  private val sizeLimit = 10000

  private var textLength: Int = 0

  override def register(str: String): Unit =
    textLength += str.length

  override def recurseLimit =
    val freeFraction: Double = ((sizeLimit - textLength) max 0).toDouble / sizeLimit
    (initialRecurseLimit * freeFraction).toInt


