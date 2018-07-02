package dotty.tools
package dottydoc

import dotc.fromtasty.ReadTastyTreesFromClasses
import dotc.typer.{FrontEnd, Typer}
import dotc.core.Contexts.Context
import dotc.CompilationUnit

import util.syntax.ContextWithContextDottydoc

/** `DocFrontEnd` uses the Dotty `FrontEnd` without discarding the AnyVal
 *  interfaces for Boolean, Int, Char, Long, Byte etc.
 *
 *  If `-from-tasty` is set, then the trees and documentation will be loaded
 *  from TASTY. The comments will be cooked after being unpickled.
 *
 *  It currently still throws away Java sources by overriding
 *  `discardAfterTyper`.
 */
class DocFrontEnd extends FrontEnd {

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    if (ctx.settings.fromTasty.value) {
      val fromTastyFrontend = new ReadTastyTreesFromClasses
      val unpickledUnits = fromTastyFrontend.runOn(units)

      val typer = new Typer()
      if (ctx.settings.YcookComments.value) {
        ctx.docbase.docstrings.keys.foreach { sym =>
            val owner = sym.owner
            val cookingCtx = ctx.withOwner(owner)
            typer.cookComment(sym, owner)(cookingCtx)
        }
      }

      unpickledUnits
    } else {
      super.runOn(units)
    }
  }

  override protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava
}
