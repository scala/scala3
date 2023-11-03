package dotty.tools
package dotc
package printing

import core.*
import Contexts.*
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
    // 10'000 -     0 / 10'0000 = 100% free
    // 10'000 -   200 / 10'0000 =  98% free * 50 = 49
    // 10'000 - 1'000 / 10'0000 =  90% free * 50 = 45
    // 10'000 - 2'000 / 10'0000 =  80% free * 50 = 40
    // every 200 characters consumes a "recurseCount"
    // which, additionally, is lowered from 100 to 50 here
    (initialRecurseLimit * freeFraction).toInt


