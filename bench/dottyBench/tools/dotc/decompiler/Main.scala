package dottyBench.tools.dotc.decompiler

import java.nio.file.Files

import dottyBench.tools.dotc
import dottyBench.tools.dotc.core.Contexts._

/** Main class of the `dotc -decompiler` decompiler.
 *
 * @author Nicolas Stucki
 */
object Main extends dotc.Driver {
  override protected def newCompiler(using Context): dotc.Compiler = {
    assert(ctx.settings.fromTasty.value)
    if (!ctx.settings.outputDir.isDefault)
      Files.deleteIfExists(ctx.settings.outputDir.value.fileNamed("decompiled.scala").jpath)
    new TASTYDecompiler
  }

  override def setup(args0: Array[String], rootCtx: Context): (List[String], Context) = {
    var args = args0.filter(a => a != "-decompile")
    if (!args.contains("-from-tasty")) args = "-from-tasty" +: args
    if (args.contains("-d")) args = "-color:never" +: args
    super.setup(args, rootCtx)
  }
}
