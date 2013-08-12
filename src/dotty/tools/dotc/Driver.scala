package dotty.tools.dotc

import config.CompilerCommand
import core.Contexts.{Context, ContextBase}
import core.DotClass

abstract class Driver extends DotClass {

  val prompt = "\ndotc>"

  protected def newCompiler(): Compiler

  protected def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context) =
    if (fileNames.nonEmpty) {
      val run = compiler.newRun
      run.compile(fileNames)
      ctx.reporter.printSummary
    }

  protected def initCtx = (new ContextBase).initialCtx

  def process(args: Array[String]): Boolean = {
    val summary = CompilerCommand.distill(args)(initCtx)
    implicit val ctx = initCtx.fresh.withSettings(summary.sstate)
    val fileNames = CompilerCommand.checkUsage(summary)
    try {
      doCompile(newCompiler(), fileNames)
      !ctx.reporter.hasErrors
    } catch {
      case ex: Throwable =>
        ctx.error(ex.getMessage)
        ex match {
          case ex: FatalError  => false // signals that we should fail compilation.
          case _               => throw ex // unexpected error, tell the outside world.
        }
    }
  }

  def main(args: Array[String]): Unit =
    sys.exit(if (process(args)) 1 else 0)
}

class FatalError(msg: String) extends Exception

