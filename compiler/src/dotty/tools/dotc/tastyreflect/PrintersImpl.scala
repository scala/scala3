package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Contexts

import scala.tasty.util.{Show, ShowExtractors, ShowSourceCode}

trait PrintersImpl extends scala.tasty.reflect.Printers with scala.tasty.reflect.TastyCore { tasty: TastyImpl =>

  def showExtractors: Show[tasty.type] = new ShowExtractors[tasty.type](this)

  def showSourceCode: Show[tasty.type] = new ShowSourceCode[tasty.type](this)

  /** Adds `show` as an extension method of a `Tree` */
  def TreeShowDeco(tree: Tree): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showTree(tree)
  }

  /** Adds `show` as an extension method of a `TypeOrBoundsTree` */
  def TypeOrBoundsTreeShowDeco(tpt: TypeOrBoundsTree): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showTypeOrBoundsTree(tpt)
  }

  /** Adds `show` as an extension method of a `TypeOrBounds` */
  def TypeOrBoundsShowDeco(tpe: TypeOrBounds): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showTypeOrBounds(tpe)
  }

  /** Adds `show` as an extension method of a `CaseDef` */
  def CaseDefShowDeco(caseDef: CaseDef): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showCaseDef(caseDef)
  }

  /** Adds `show` as an extension method of a `Pattern` */
  def PatternShowDeco(pattern: Pattern): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showPattern(pattern)
  }

  /** Adds `show` as an extension method of a `Constant` */
  def ConstantShowDeco(const: Constant): ShowAPI = new ShowAPI {
    def show(implicit ctx: Contexts.Context, s: Show[tasty.type]): String = s.showConstant(const)
  }

}
