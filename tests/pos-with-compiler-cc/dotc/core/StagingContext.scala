package dotty.tools.dotc.core

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.transform.PCPCheckAndHeal

object StagingContext {

  /** A key to be used in a context property that tracks the quotation level */
  private val QuotationLevel = new Property.Key[Int]

  /** A key to be used in a context property that tracks the quotation stack.
   *  Stack containing the Quotes references received by the surrounding quotes.
   */
  private val QuotesStack = new Property.Key[List[tpd.Tree]]

  private val TaggedTypes = new Property.Key[PCPCheckAndHeal.QuoteTypeTags]

  /** All enclosing calls that are currently inlined, from innermost to outermost. */
  def level(using Context): Int =
    ctx.property(QuotationLevel).getOrElse(0)

  /** Context with an incremented quotation level. */
  def quoteContext(using Context): Context =
    ctx.fresh.setProperty(QuotationLevel, level + 1)

  /** Context with an incremented quotation level and pushes a reference to a Quotes on the quote context stack */
  def pushQuotes(qctxRef: tpd.Tree)(using Context): Context =
    val old = ctx.property(QuotesStack).getOrElse(List.empty)
    ctx.fresh.setProperty(QuotationLevel, level + 1)
             .setProperty(QuotesStack, qctxRef :: old)

  /** Context with a decremented quotation level. */
  def spliceContext(using Context): Context =
    ctx.fresh.setProperty(QuotationLevel, level - 1)

  def contextWithQuoteTypeTags(taggedTypes: PCPCheckAndHeal.QuoteTypeTags)(using Context) =
    ctx.fresh.setProperty(TaggedTypes, taggedTypes)

  def getQuoteTypeTags(using Context): PCPCheckAndHeal.QuoteTypeTags =
    ctx.property(TaggedTypes).get

  /** Context with a decremented quotation level and pops the Some of top of the quote context stack or None if the stack is empty.
   *  The quotation stack could be empty if we are in a top level splice or an erroneous splice directly within a top level splice.
   */
  def popQuotes()(using Context): (Option[tpd.Tree], Context) =
    val ctx1 = ctx.fresh.setProperty(QuotationLevel, level - 1)
    val head =
      ctx.property(QuotesStack) match
        case Some(x :: xs) =>
          ctx1.setProperty(QuotesStack, xs)
          Some(x)
        case _ =>
          None // Splice at level 0 or lower
    (head, ctx1)
}
