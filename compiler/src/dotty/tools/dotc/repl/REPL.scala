package dotty.tools
package dotc
package repl

import core.Contexts.Context
import reporting.Reporter
import io.{AbstractFile, PlainFile, VirtualDirectory}
import dotty.tools.io.{PlainDirectory, Directory}
import java.io.{BufferedReader, File => JFile, FileReader, PrintWriter}
import java.net.{URL, URLClassLoader}

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
    new repl.CompilingInterpreter(config.output, ctx, config.classLoader)

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
    val continuationPrompt = "       "
    val version            = ".next (pre-alpha)"

    def context(ctx: Context): Context = ctx

    /** The first interpreted commands always take a couple of seconds due to
     *  classloading. To bridge the gap, we warm up the interpreter by letting
     *  it interpret at least a dummy line while waiting for the first line of
     *  input to be entered.
     */
    val initialCommands: List[String] =
      "val theAnswerToLifeInTheUniverseAndEverything = 21 * 2" :: Nil

    /** Before exiting, the interpreter will also run the cleanup commands
     *  issued in the variable below. This is useful if your REPL creates
     *  things during its run that should be dealt with before shutdown.
     */
    val cleanupCommands: List[String] = Nil

    /** Initial values in the REPL can also be bound from runtime. Override
     *  this variable in the following manner to bind a variable at the start
     *  of the REPL session:
     *
     *  {{{
     *  override val boundValues = Array("exampleList" -> List(1, 1, 2, 3, 5))
     *  }}}
     *
     *  This is useful if you've integrated the REPL as part of your project
     *  and already have objects available during runtime that you'd like to
     *  inspect.
     */
    val boundValues: Array[(String, Any)] = Array.empty[(String, Any)]

    /** To pass a custom ClassLoader to the Dotty REPL, overwride this value */
    val classLoader: Option[ClassLoader] = None

    /** The default input reader */
    def input(in: Interpreter)(implicit ctx: Context): InteractiveReader = {
      val emacsShell = System.getProperty("env.emacs", "") != ""
      //println("emacsShell="+emacsShell) //debug
      if (emacsShell) new SimpleReader()
      else InteractiveReader.createDefault(in)
    }

    /** The default output writer */
    def output: PrintWriter = new NewLinePrintWriter(new ConsoleWriter, true)
  }
}
