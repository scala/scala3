package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core {

  // Symbol

  trait SymbolAPI {

    def owner(implicit ctx: Context): Symbol

    def flags(implicit ctx: Context): FlagSet

    def privateWithin(implicit ctx: Context): Option[Type]
    def protectedWithin(implicit ctx: Context): Option[Type]

    def name(implicit ctx: Context): String
    def fullName(implicit ctx: Context): String

    def localContext(implicit ctx: Context): Context

    def asPackage(implicit ctx: Context): PackageSymbol
    def asClass(implicit ctx: Context): ClassSymbol
    def asDef(implicit ctx: Context): DefSymbol
    def asVal(implicit ctx: Context): ValSymbol
    def asType(implicit ctx: Context): TypeSymbol
    def asBind(implicit ctx: Context): BindSymbol

    def annots(implicit ctx: Context): List[Term]

  }
  implicit def SymbolDeco(symbol: Symbol): SymbolAPI

  // PackageSymbol

  val IsPackageSymbol: IsPackageSymbolExtractor
  abstract class IsPackageSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[PackageSymbol]
  }

  trait PackageSymbolAPI {
    def tree(implicit ctx: Context): PackageDef
  }
  implicit def PackageSymbolDeco(symbol: PackageSymbol): PackageSymbolAPI

  // ClassSymbol

  val IsClassSymbol: IsClassSymbolExtractor
  abstract class IsClassSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ClassSymbol]
  }

  trait ClassSymbolAPI {
    def tree(implicit ctx: Context): ClassDef
  }
  implicit def ClassSymbolDeco(symbol: ClassSymbol): ClassSymbolAPI

  // TypeSymbol

  val IsTypeSymbol: IsTypeSymbolExtractor
  abstract class IsTypeSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol]
  }

  trait TypeSymbolAPI {
    def tree(implicit ctx: Context): TypeDef
  }
  implicit def TypeSymbolDeco(symbol: TypeSymbol): TypeSymbolAPI

  // DefSymbol

  val IsDefSymbol: IsDefSymbolExtractor
  abstract class IsDefSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[DefSymbol]
  }

  trait DefSymbolAPI {
    def tree(implicit ctx: Context): DefDef
  }
  implicit def DefSymbolDeco(symbol: DefSymbol): DefSymbolAPI

  // ValSymbol

  val IsValSymbol: IsValSymbolExtractor
  abstract class IsValSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ValSymbol]
  }

  trait ValSymbolAPI {
    def tree(implicit ctx: Context): ValDef
  }
  implicit def ValSymbolDeco(symbol: ValSymbol): ValSymbolAPI

  // BindSymbol

  val IsBindSymbol: IsBindSymbolExtractor
  abstract class IsBindSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol]
  }

  trait BindSymbolAPI {
    def tree(implicit ctx: Context): Bind
  }
  implicit def BindSymbolDeco(symbol: BindSymbol): BindSymbolAPI

  // NoSymbol

  val NoSymbol: NoSymbolExtractor
  abstract class NoSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Boolean
  }
}
