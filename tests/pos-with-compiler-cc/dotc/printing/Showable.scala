package dotty.tools.dotc
package printing

import core._

import Contexts._, Texts._, Decorators._
import config.Config.summarizeDepth

trait Showable extends Any {

  /** The text representation of this showable element.
   *  This normally dispatches to a pattern matching
   *  method in Printers.
   */
  def toText(printer: Printer): Text

  /** A fallback text representation, if the pattern matching
   *  in Printers does not have a case for this showable element
   */
  def fallbackToText(printer: Printer): Text = toString

  /** The string representation of this showable element. */
  def show(using Context): String = toText(ctx.printer).show

  /** The string representation with each line after the first one indented
   *  by the given given margin (in spaces).
   */
  def showIndented(margin: Int)(using Context): String = show.replace("\n", "\n" + " " * margin).nn

  /** The summarized string representation of this showable element.
   *  Recursion depth is limited to some smallish value. Default is
   *  Config.summarizeDepth.
   */
  def showSummary(depth: Int = summarizeDepth)(using Context): String =
    show(using ctx.fresh.setProperty(MessageLimiter, SummarizeMessageLimiter(depth)))
}
