package scala.tasty
package reflect

// TODO use QuoteContext instead of Reflection
trait Printer[R <: Reflection & Singleton] {

  /** Instance of reflection interface */
  val tasty: R

  /** Show a String representation of a tasty.Tree */
  def showTree(tree: tasty.Tree): String

  /** Show a String representation of a tasty.Type */
  def showType(tpe: tasty.Type): String

  /** Show a String representation of a tasty.Constant */
  def showConstant(const: tasty.Constant): String

  /** Show a String representation of a tasty.Symbol */
  def showSymbol(symbol: tasty.Symbol): String

  /** Show a String representation of a tasty.Flags */
  def showFlags(flags: tasty.Flags): String
}
