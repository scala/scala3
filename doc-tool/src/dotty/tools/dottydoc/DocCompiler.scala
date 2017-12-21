package dotty.tools
package dottydoc

import core._
import core.transform._
import dotc.core.Phases.Phase
import dotc.Compiler

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

  override protected def frontendPhases: List[List[Phase]] =
    List(new DocFrontEnd) :: Nil

  override protected def picklerPhases: List[List[Phase]] =
    Nil

  override protected def transformPhases: List[List[Phase]] =
    List(new DocImplicitsPhase) ::
    List(new DocASTPhase) ::
    List(DocMiniTransformations(new UsecasePhase,
                                new DocstringPhase,
                                new PackageObjectsPhase,
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
