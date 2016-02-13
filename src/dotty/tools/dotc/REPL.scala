package dotty.tools
package dotc

import core.Phases
import core.Contexts.Context
import reporting.Reporter
import java.io.EOFException
import scala.annotation.tailrec
import io.VirtualDirectory
import java.io.{BufferedReader, File, FileReader, PrintWriter}
import repl._

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

  def input(implicit ctx: Context): InteractiveReader = {
    val emacsShell = System.getProperty("env.emacs", "") != ""
    //println("emacsShell="+emacsShell) //debug
    if (ctx.settings.Xnojline.value || emacsShell) new SimpleReader()
    else InteractiveReader.createDefault()
  }

  def output: PrintWriter = new NewLinePrintWriter(new ConsoleWriter, true)

  override def newCompiler(): Compiler = new repl.Interpreter(output)

  override def sourcesRequired = false

  override def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter = {
    new InterpreterLoop(compiler, input, output).run()
    ctx.reporter
  }
}
