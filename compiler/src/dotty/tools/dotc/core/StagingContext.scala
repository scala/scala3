package dotty.tools.dotc.core

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.staging.PCPCheckAndHeal
import dotty.tools.dotc.staging.StagingLevel.*

object StagingContext {

  /** A key to be used in a context property that tracks the quotation stack.
   *  Stack containing the Quotes references received by the surrounding quotes.
   */
  private val QuotesStack = new Property.Key[List[tpd.Tree]]

  private val TaggedTypes = new Property.Key[PCPCheckAndHeal.QuoteTypeTags]

  /** Context with an incremented quotation level and pushes a reference to a Quotes on the quote context stack */
  def pushQuotes(qctxRef: tpd.Tree)(using Context): Context =
    val old = ctx.property(QuotesStack).getOrElse(List.empty)
    quoteContext.setProperty(QuotesStack, qctxRef :: old)

  def contextWithQuoteTypeTags(taggedTypes: PCPCheckAndHeal.QuoteTypeTags)(using Context) =
    ctx.fresh.setProperty(TaggedTypes, taggedTypes)

  def getQuoteTypeTags(using Context): PCPCheckAndHeal.QuoteTypeTags =
    ctx.property(TaggedTypes).get

  /** Context with a decremented quotation level and pops the Some of top of the quote context stack or None if the stack is empty.
   *  The quotation stack could be empty if we are in a top level splice or an erroneous splice directly within a top level splice.
   */
  def popQuotes()(using Context): (Option[tpd.Tree], Context) =
    val ctx1 = spliceContext
    val head =
      ctx.property(QuotesStack) match
        case Some(x :: xs) =>
          ctx1.setProperty(QuotesStack, xs)
          Some(x)
        case _ =>
          None // Splice at level 0 or lower
    (head, ctx1)
}
