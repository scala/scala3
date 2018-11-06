package scala.tasty
package reflect

import scala.tasty.util.Show

trait Printers { reflect: Reflection =>
  // TASTy 🎂 needed to for the path dependency `tasty.type` to make sure the
  // implicit printers of different instances of Tasty are not used.

  /** Printer that prints the tree as extractors (enabled by default) */
  implicit def showExtractors: Show[reflect.type]

  /** Printer that prints the tree as source code */
  def showSourceCode: Show[reflect.type]

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
    /** Shows the string representation based on an implicit instance of `Show[tasty.type]`
     *  See: `showExtractors` and `showSourceCode`
     */
    def show(implicit ctx: Context, s: Show[reflect.type]): String
  }

}
