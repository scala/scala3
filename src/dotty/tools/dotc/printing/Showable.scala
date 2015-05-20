package dotty.tools.dotc
package printing

import core._

import Contexts._, Texts._, Decorators._
import config.Config.summarizeDepth
import scala.util.control.NonFatal

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
  def show(implicit ctx: Context): String =
    try toText(ctx.printer).show
    catch {
      case NonFatal(ex) => s"[cannot display due to $ex, raw string = $toString]"
    }

  /** The summarized string representation of this showable element.
   *  Recursion depth is limited to some smallish value. Default is
   *  Config.summarizeDepth.
   */
  def showSummary(depth: Int)(implicit ctx: Context): String =
    ctx.printer.summarized(depth)(show)

  def showSummary(implicit ctx: Context): String = showSummary(summarizeDepth)
}
