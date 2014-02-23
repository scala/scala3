package dotty.compiler
package internal
package printing

import core._

import Contexts._, Texts._, Decorators._

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
  def show(implicit ctx: Context): String = toText(ctx.printer).show
}
