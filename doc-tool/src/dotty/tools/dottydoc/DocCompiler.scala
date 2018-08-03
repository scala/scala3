package dotty.tools
package dottydoc

import core._
import core.transform._
import dotc.core.Contexts.Context
import dotc.core.Phases.Phase
import dotc.core.Mode
import dotc.{Compiler, Run}

import dotty.tools.dotc.fromtasty.{ReadTastyTreesFromClasses, TASTYRun}
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

  override def newRun(implicit ctx: Context): Run = {
    if (ctx.settings.fromTasty.value) {
      reset()
      new TASTYRun(this, ctx.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))
    } else {
      super.newRun
    }
  }

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTastyTreesFromClasses) ::
    List(new DocFrontEnd) ::  Nil

  override protected def picklerPhases: List[List[Phase]] =
    Nil

  override protected def transformPhases: List[List[Phase]] =
    List(new CookComments) ::
    List(new DocImplicitsPhase) ::
    List(new DocASTPhase) ::
    List(DocMiniTransformations(new UsecasePhase,
                                new DocstringPhase)) ::
    List(DocMiniTransformations(new PackageObjectsPhase,
                                new LinkReturnTypes,
                                new LinkParamListTypes,
                                new LinkImplicitlyAddedTypes,
                                new LinkSuperTypes,
                                new LinkCompanions,
                                new AlternateConstructors,
                                new SortMembers)) ::
    List(DocMiniTransformations(new RemoveEmptyPackages)) ::
    Nil

  override protected def backendPhases: List[List[Phase]] =
    List(new StatisticsPhase) :: Nil

}
