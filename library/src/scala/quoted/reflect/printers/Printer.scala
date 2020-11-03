package scala.quoted
package reflect.printers

trait Printer {
  /** Show a String representation of a reflect.Tree */
  def showTree(using QuoteContext)(tree: qctx.reflect.Tree): String

  /** Show a String representation of a reflect.Type */
  def showType(using QuoteContext)(tpe: qctx.reflect.TypeRepr): String

  /** Show a String representation of a reflect.Constant */
  def showConstant(using QuoteContext)(const: qctx.reflect.Constant): String

  /** Show a String representation of a reflect.Symbol */
  def showSymbol(using QuoteContext)(symbol: qctx.reflect.Symbol): String

  /** Show a String representation of a reflect.Flags */
  def showFlags(using QuoteContext)(flags: qctx.reflect.Flags): String
}
