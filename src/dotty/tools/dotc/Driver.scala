package dotty.tools.dotc

import config.CompilerCommand
import core.Contexts.{Context, ContextBase}
import util.DotClass
import reporting._
import scala.util.control.NonFatal

abstract class Driver extends DotClass {

  val prompt = "\ndotc>"

  protected def newCompiler(): Compiler

  protected def emptyReporter: Reporter = new StoreReporter

  protected def doCompile(compiler: Compiler, fileNames: List[String], reporter: Option[Reporter] = None)
      (implicit ctx: Context): Reporter =
    if (fileNames.nonEmpty) {
      val run = compiler.newRun(ctx, reporter)
      run.compile(fileNames)
      run.printSummary()
    } else emptyReporter

  protected def initCtx = (new ContextBase).initialCtx

  def process(args: Array[String], rootCtx: Context, reporter: Option[Reporter] = None): Reporter = {
    val summary = CompilerCommand.distill(args)(rootCtx)
    implicit val ctx: Context = initCtx.fresh.setSettings(summary.sstate)
    val fileNames = CompilerCommand.checkUsage(summary)
    try {
      doCompile(newCompiler(), fileNames, reporter)
    } catch {
      case ex: FatalError  =>
        ctx.error(ex.getMessage) // signals that we should fail compilation.
        ctx.typerState.reporter
    }
  }

  def main(args: Array[String]): Unit =
    sys.exit(if (process(args, initCtx).hasErrors) 1 else 0)
}

class FatalError(msg: String) extends Exception

