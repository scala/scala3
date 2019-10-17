package scala.tasty
package reflect

trait Printer[R <: Reflection & Singleton] {

  /** Instance of reflection interface */
  val tasty: R

  /** Show a String representation of a tasty.Tree */
  def showTree(tree: tasty.Tree)(given ctx: tasty.Context): String

  /** Show a String representation of a tasty.TypeOrBounds */
  def showTypeOrBounds(tpe: tasty.TypeOrBounds)(given ctx: tasty.Context): String

  /** Show a String representation of a tasty.Constant */
  def showConstant(const: tasty.Constant)(given ctx: tasty.Context): String

  /** Show a String representation of a tasty.Symbol */
  def showSymbol(symbol: tasty.Symbol)(given ctx: tasty.Context): String

  /** Show a String representation of a tasty.Flags */
  def showFlags(flags: tasty.Flags)(given ctx: tasty.Context): String
}
