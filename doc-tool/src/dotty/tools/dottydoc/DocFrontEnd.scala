package dotty.tools
package dottydoc

import dotc.fromtasty.ReadTastyTreesFromClasses
import dotc.typer.{FrontEnd, Typer}
import dotc.core.Contexts.ContextRenamed
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

  override def runOn(units: List[CompilationUnit])(implicit ctx: ContextRenamed): List[CompilationUnit] = {
    if (ctx.settings.fromTasty.value) units
    else super.runOn(units)
  }

  override protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: ContextRenamed) =
    unit.isJava
}
