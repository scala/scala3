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

    /** Name of the definition */
    def name(implicit ctx: Context): String

    /** Full name of the definition from the _root_ package */
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

  val ClassSymbol: ClassSymbolModule
  abstract class ClassSymbolModule {
    /** The ClassSymbol of a global class definition */
    def of(fullName: String)(implicit ctx: Context): ClassSymbol
  }

  trait ClassSymbolAPI {
    /** Tree of this class definition */
    def tree(implicit ctx: Context): ClassDef

    /** Fields directly declared in the class */
    def fields(implicit ctx: Context): List[Symbol]

    /** Field with the given name directly declared in the class */
    def field(name: String)(implicit ctx: Context): Option[Symbol]

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(implicit ctx: Context): List[DefSymbol]

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(implicit ctx: Context): List[DefSymbol]

    /** Get named non-private methods declared or inherited */
    def method(name: String)(implicit ctx: Context): List[DefSymbol]

    /** Get all non-private methods declared or inherited */
    def methods(implicit ctx: Context): List[DefSymbol]

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(implicit ctx: Context): List[ValSymbol]

    /** The class symbol of the companion module class */
    def companionClass(implicit ctx: Context): Option[ClassSymbol]

    /** The symbol of the companion module */
    def companionModule(implicit ctx: Context): Option[ValSymbol]

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

    /** The class symbol of the companion module class */
    def companionClass(implicit ctx: Context): Option[ClassSymbol]

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
