package dotty.tools
package dotc
package repl

import core.Contexts.Context
import reporting.Reporter
import java.io.{BufferedReader, File, FileReader, PrintWriter}

/** A compiler which stays resident between runs.
 *  Usage:
 *
 *  > scala dotty.tools.dotc.Resident <options> <initial files>
 *
 *  dotc> "more options and files to compile"
 *
 *  ...
 *
 *  dotc> :reset  // reset all options to the ones passed on the command line
 *
 *  ...
 *
 *  dotc> :q     // quit
 */
class REPL extends Driver {

  lazy val config = new REPL.Config

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val (strs, ctx) = super.setup(args, rootCtx)
    (strs, config.context(ctx))
  }

  override def newCompiler(implicit ctx: Context): Compiler =
    new repl.CompilingInterpreter(config.output, ctx)

  override def sourcesRequired = false

  override def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter = {
    if (fileNames.isEmpty)
      new InterpreterLoop(compiler, config).run()
    else
      ctx.error(s"don't now what to do with $fileNames%, %")
    ctx.reporter
  }
}

object REPL {
  class Config {
    val prompt             = "scala> "
    val continuationPrompt = "     | "
    val version            = ".next (pre-alpha)"

    def context(ctx: Context): Context = ctx

    /** The default input reader */
    def input(in: Interpreter)(implicit ctx: Context): InteractiveReader = {
      val emacsShell = System.getProperty("env.emacs", "") != ""
      //println("emacsShell="+emacsShell) //debug
      if (ctx.settings.Xnojline.value || emacsShell) new SimpleReader()
      else InteractiveReader.createDefault(in)
    }

    /** The default output writer */
    def output: PrintWriter = new NewLinePrintWriter(new ConsoleWriter, true)
  }
}
