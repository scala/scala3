package dotty.tools
package dotc

import core.Contexts._
import reporting.Reporter
import java.io.EOFException
import scala.annotation.tailrec

/** A compiler which stays resident between runs. This is more of a PoC than
 *  something that's expected to be used often
 *
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
class Resident extends Driver {

  object residentCompiler extends Compiler

  override def sourcesRequired: Boolean = false

  private val quit = ":q"
  private val reset = ":reset"
  private val prompt = "dotc> "

  private def getLine() = {
    Console.print(prompt)
    try scala.io.StdIn.readLine() catch { case _: EOFException => quit }
  }

  final override def process(args: Array[String], rootCtx: Context): Reporter = {
    @tailrec def loop(args: Array[String], prevCtx: Context): Reporter = {
      var (possibleFiles, ctx) = setup(args, prevCtx)
      if possibleFiles.isDefined then
        inContext(ctx) {
          doCompile(residentCompiler, possibleFiles.get) // using more complex constructs like fold or map instead of get will make @tailrec complain
        }
        var nextCtx = ctx
        var line = getLine()
        while (line == reset) {
          nextCtx = rootCtx
          line = getLine()
        }
        if (line.startsWith(quit)) ctx.reporter
        else loop(line split "\\s+", nextCtx)
      else
        ctx.reporter
    }
    loop(args, rootCtx)
  }
}
