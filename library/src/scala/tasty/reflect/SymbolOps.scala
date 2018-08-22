package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends TastyCore {

  trait SymbolAPI {
    def isEmpty(implicit ctx: Context): Boolean
    def isClass(implicit ctx: Context): Boolean
    def name(implicit ctx: Context): String
    def fullName(implicit ctx: Context): String
    def localContext(implicit ctx: Context): Context
    def tree(implicit ctx: Context): Option[Definition]
  }
  implicit def SymbolDeco(symbol: Symbol): SymbolAPI

}
