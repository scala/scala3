package scala.tasty
package reflect

// TODO use QuoteContext instead of Reflection
trait Printer[R <: Reflection & Singleton] {

  /** Instance of reflection interface */
  val reflect: R
  import reflect._

  /** Show a String representation of a reflect.Tree */
  def showTree(tree: Tree): String

  /** Show a String representation of a reflect.Type */
  def showType(tpe: TypeRepr): String

  /** Show a String representation of a reflect.Constant */
  def showConstant(const: Constant): String

  /** Show a String representation of a reflect.Symbol */
  def showSymbol(symbol: Symbol): String

  /** Show a String representation of a reflect.Flags */
  def showFlags(flags: Flags): String
}
