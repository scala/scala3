package dotty.tools
package dottydoc

import core._
import core.transform._
import dotc.config.CompilerCommand
import dotc.config.Printers.dottydoc
import dotc.core.Contexts._
import dotc.core.Phases.Phase
import dotc.typer.FrontEnd
import dotc.{ CompilationUnit, Compiler, Driver, Run }
import io.PlainFile
import model.Package
import model.json._

import _root_.java.util.{ Map => JMap }

/** Custom Compiler with phases for the documentation tool
 *
 *  The idea here is to structure `dottydoc` around the new infrastructure. As
 *  such, dottydoc will itself be a compiler. It will, however, produce a format
 *  that can be used by other tools or web-browsers.
 *
 *  Example:
 *    1. Use the existing FrontEnd to typecheck the code being fed to dottydoc
 *    2. Create an AST that is serializable
 *    3. Serialize to JS object
 */
class DocCompiler extends Compiler {
  override def phases: List[List[Phase]] = List(
    List(new DocFrontEnd),
    List(new DocImplicitsPhase),
    List(new DocASTPhase),
    List(DocMiniTransformations(new LinkReturnTypes,
                                new LinkParamListTypes,
                                new LinkImplicitlyAddedTypes,
                                new SortMembers))
  )
}

class DocFrontEnd extends FrontEnd {
  override protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava
}

abstract class DocDriver extends Driver {
  import scala.collection.JavaConverters._

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx     = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)

    ctx.setSettings(summary.sstate)
    ctx.setSetting(ctx.settings.YkeepComments, true)

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    (fileNames, ctx)
  }

  override def newCompiler(implicit ctx: Context): Compiler = new DocCompiler


  def compiledDocs(args: Array[String]): collection.Map[String, Package] = {
    val (fileNames, ctx) = setup(args, initCtx.fresh)
    doCompile(newCompiler(ctx), fileNames)(ctx)

    ctx.docbase.packages[Package]
  }

  def compiledDocsJava(args: Array[String]): JMap[String, Package] =
    compiledDocs(args).asJava

  def indexToJson(index: JMap[String, Package]): String =
    index.asScala.json
}
