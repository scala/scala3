package dotty.tools
package dottydoc

import core._
import core.transform._
import dotc.core.Contexts.{Context, ctx}
import dotc.core.Phases.Phase
import dotc.core.Mode
import dotc.{Compiler, CompilationUnit, Run}
import dotc.typer.FrontEnd

import dotty.tools.dotc.fromtasty.{ReadTasty, TASTYRun}
import dotty.tools.dotc.transform.CookComments

/** Custom Compiler with phases for the documentation tool
 *
 *  The idea here is to structure `dottydoc` around the new infrastructure. As
 *  such, dottydoc will itself be a compiler. It will, however, produce a format
 *  that can be used by other tools or web-browsers.
 *
 *  Example:
 *  1. Use the existing FrontEnd to typecheck the code being fed to dottydoc,
 *     wihtout discarding AnyVal interfaces
 *  2. Create an AST that is serializable
 *  3. Serialize to JS object
 */
class DocCompiler extends Compiler {

  override def newRun(using Context): Run = {
    if (ctx.settings.fromTasty.value) {
      reset()
      new TASTYRun(this, ctx.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))
    }
    else
      super.newRun
  }

  /** `DocFrontEnd` uses the Dotty `FrontEnd` without discarding the AnyVal
   *  interfaces for Boolean, Int, Char, Long, Byte etc.
   *
   *  If `-from-tasty` is set, then the trees and documentation will be loaded
   *  from TASTY. The comments will be cooked after being unpickled.
   *
   *  It currently still throws away Java sources by overriding
   *  `discardAfterTyper`.
   */
  private class DocFrontEnd extends FrontEnd {
    override protected def discardAfterTyper(unit: CompilationUnit)(using Context) =
      unit.isJava

    override def isRunnable(using Context): Boolean =
      super.isRunnable && !ctx.settings.fromTasty.value
  }

  override def phases: List[List[Phase]] = List(
    List(new DocFrontEnd),
    List(new ReadTasty),
    List(new CookComments),
    List(new DocImplicitsPhase),
    List(new DocASTPhase),
    List(DocMiniTransformations(
      new UsecasePhase,
      new DocstringPhase)),
    List(DocMiniTransformations(
      new PackageObjectsPhase,
      new LinkReturnTypes,
      new LinkParamListTypes,
      new LinkImplicitlyAddedTypes,
      new LinkSuperTypes,
      new LinkCompanions,
      new AlternateConstructors,
      new SortMembers)),
    List(DocMiniTransformations(
      new RemoveEmptyPackages)),
    List(new StatisticsPhase)
  )
}
