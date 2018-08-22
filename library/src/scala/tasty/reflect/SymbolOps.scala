package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends TastyCore {

  trait SymbolAPI {
    def isEmpty: Boolean
    def localContext(implicit ctx: Context): Context
    def tree(implicit ctx: Context): Option[Definition]
  }
  implicit def SymbolDeco(symbol: Symbol): SymbolAPI

}
