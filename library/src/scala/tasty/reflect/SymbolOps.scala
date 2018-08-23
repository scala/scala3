package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends TastyCore {

  trait SymbolAPI {

    def owner(implicit ctx: Context): Symbol

    def isEmpty(implicit ctx: Context): Boolean
    def isClass(implicit ctx: Context): Boolean

    def flags(implicit ctx: Context): FlagSet

    def privateWithin(implicit ctx: Context): Option[Type]
    def protectedWithin(implicit ctx: Context): Option[Type]

    def name(implicit ctx: Context): String
    def fullName(implicit ctx: Context): String

    def localContext(implicit ctx: Context): Context

    def tree(implicit ctx: Context): Option[Definition]

    def annots(implicit ctx: Context): List[Term]

  }
  implicit def SymbolDeco(symbol: Symbol): SymbolAPI

}
