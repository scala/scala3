package dotty.tools
package dottydoc

import dotty.tools.dottydoc.util.syntax._
import core.ContextDottydoc
import dotc.core.Contexts._
import dotc.{ Compiler, Driver }
import model.Package
import model.json._
import dotc.config._
import dotc.core.Comments.ContextDoc

/** `DocDriver` implements the main entry point to the Dotty documentation
  * tool. It's methods are used by the external scala and java APIs.
  */
class DocDriver extends Driver {
  import _root_.java.util.{ Map => JMap }
  import scala.collection.JavaConverters._

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx     = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)

    ctx.setSettings(summary.sstate)
    ctx.setSetting(ctx.settings.YkeepComments, true)
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    (fileNames, ctx)
  }

  override def newCompiler(implicit ctx: Context): Compiler = new DocCompiler

  def compiledDocs(args: Array[String]): collection.Map[String, Package] = {
    val (fileNames, ctx) = setup(args, initCtx.fresh)
    doCompile(newCompiler(ctx), fileNames)(ctx)

    ctx.docbase.packages
  }

  def compiledDocsJava(args: Array[String]): JMap[String, Package] =
    compiledDocs(args).asJava

  def indexToJson(index: collection.Map[String, Package]): String =
    index.json

  def indexToJsonJava(index: JMap[String, Package]): String =
    indexToJson(index.asScala)
}
