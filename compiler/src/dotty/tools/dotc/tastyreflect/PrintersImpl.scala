package dotty.tools.dotc.tastyreflect

import scala.tasty.util.{Printer, ExtractorsPrinter, SourceCodePrinter}

trait PrintersImpl extends scala.tasty.reflect.Printers with scala.tasty.reflect.ReflectionCore { reflect: ReflectionImpl =>

  def showExtractors: Printer[reflect.type] = new ExtractorsPrinter[reflect.type](this)

  def showSourceCode: Printer[reflect.type] = new SourceCodePrinter[reflect.type](this)

  /** Adds `show` as an extension method of a `Tree` */
  def TreeShowDeco(tree: Tree): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showTree(tree)
    def showCode(implicit ctx: Context): String = showSourceCode.showTree(tree)
  }

  /** Adds `show` as an extension method of a `TypeOrBoundsTree` */
  def TypeOrBoundsTreeShowDeco(tpt: TypeOrBoundsTree): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showTypeOrBoundsTree(tpt)
    def showCode(implicit ctx: Context): String = showSourceCode.showTypeOrBoundsTree(tpt)
  }

  /** Adds `show` as an extension method of a `TypeOrBounds` */
  def TypeOrBoundsShowDeco(tpe: TypeOrBounds): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showTypeOrBounds(tpe)
    def showCode(implicit ctx: Context): String = showSourceCode.showTypeOrBounds(tpe)
  }

  /** Adds `show` as an extension method of a `CaseDef` */
  def CaseDefShowDeco(caseDef: CaseDef): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showCaseDef(caseDef)
    def showCode(implicit ctx: Context): String = showSourceCode.showCaseDef(caseDef)
  }

  /** Adds `show` as an extension method of a `Pattern` */
  def PatternShowDeco(pattern: Pattern): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showPattern(pattern)
    def showCode(implicit ctx: Context): String = showSourceCode.showPattern(pattern)
  }

  /** Adds `show` as an extension method of a `Constant` */
  def ConstantShowDeco(const: Constant): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showConstant(const)
    def showCode(implicit ctx: Context): String = showSourceCode.showConstant(const)
  }

  /** Adds `show` as an extension method of a `Symbol` */
  def SymbolShowDeco(symbol: Symbol): ShowAPI = new ShowAPI {
    def show(implicit ctx: Context): String = showExtractors.showSymbol(symbol)
    def showCode(implicit ctx: Context): String = showSourceCode.showSymbol(symbol)
  }

}
