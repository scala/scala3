package scala.tasty
package reflect

trait Printer[R <: Reflection & Singleton] {

  /** Instance of reflection interface */
  val tasty: R

  /** Show a String representation of a tasty.Tree */
  def showTree(tree: tasty.Tree)(using ctx: tasty.Context): String

  /** Show a String representation of a tasty.Type */
  def showType(tpe: tasty.Type)(using ctx: tasty.Context): String

  /** Show a String representation of a tasty.Constant */
  def showConstant(const: tasty.Constant)(using ctx: tasty.Context): String

  /** Show a String representation of a tasty.Symbol */
  def showSymbol(symbol: tasty.Symbol)(using ctx: tasty.Context): String

  /** Show a String representation of a tasty.Flags */
  def showFlags(flags: tasty.Flags)(using ctx: tasty.Context): String
}
