package dotty.tools.dotc.decompiler

import java.nio.file.{Files, Paths}

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts._

/** Main class of the `dotc -decompiler` decompiler.
 *
 * @author Nicolas Stucki
 */
object Main extends dotc.Driver {
  override protected def newCompiler(implicit ctx: Context): dotc.Compiler = {
    assert(ctx.settings.fromTasty.value)
    val outputDir = ctx.settings.outputDir.value
    if (outputDir != ".")
      Files.deleteIfExists(Paths.get(outputDir + "/decompiled.scala"))
    new TASTYDecompiler
  }

  override def setup(args0: Array[String], rootCtx: Context): (List[String], Context) = {
    var args = args0.filter(a => a != "-decompile")
    if (!args.contains("-from-tasty")) args = "-from-tasty" +: args
    if (args.contains("-d")) args = "-color:never" +: args
    super.setup(args, rootCtx)
  }
}
