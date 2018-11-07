package scala.tasty
package reflect

trait Printers extends ReflectionCore {

  /** Adds `show` as an extension method of a `Tree` */
  implicit def TreeShowDeco(tree: Tree): ShowAPI

  /** Adds `show` as an extension method of a `TypeOrBoundsTree` */
  implicit def TypeOrBoundsTreeShowDeco(tpt: TypeOrBoundsTree): ShowAPI

  /** Adds `show` as an extension method of a `TypeOrBounds` */
  implicit def TypeOrBoundsShowDeco(tpt: TypeOrBounds): ShowAPI

  /** Adds `show` as an extension method of a `CaseDef` */
  implicit def CaseDefShowDeco(caseDef: CaseDef): ShowAPI

  /** Adds `show` as an extension method of a `Pattern` */
  implicit def PatternShowDeco(pattern: Pattern): ShowAPI

  /** Adds `show` as an extension method of a `Constant` */
  implicit def ConstantShowDeco(const: Constant): ShowAPI

  /** Adds `show` as an extension method of a `Symbol` */
  implicit def SymbolShowDeco(symbol: Symbol): ShowAPI

  /** Define `show` as method */
  trait ShowAPI {
    /** Shows the tree as extractors */
    def show(implicit ctx: Context): String

    /** Shows the tree as source code */
    def showCode(implicit ctx: Context): String
  }

}
