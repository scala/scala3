package dotty.tools.dotc.decompiler

import java.nio.file.Files

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.nio.*

/** Main class of the `dotc -decompiler` decompiler.
 *
 * @author Nicolas Stucki
 */
object Main extends dotc.Driver {
  override protected def newCompiler(using Context): dotc.Compiler = {
    assert(ctx.settings.fromTasty.value)
    if (!ctx.settings.outputDir.isDefault)
      ctx.settings.outputDir.value.getFile("decompiled.scala").foreach(_.delete())
    new TASTYDecompiler
  }

  override def setup(args0: Array[String], rootCtx: Context): Option[(List[File], Context)] = {
    var args = args0.filter(a => a != "-decompile")
    if (!args.contains("-from-tasty")) args = "-from-tasty" +: args
    if (args.contains("-d")) args = "-color:never" +: args
    super.setup(args, rootCtx)
  }
}
