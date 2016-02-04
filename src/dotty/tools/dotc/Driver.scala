package dotty.tools.dotc

import config.CompilerCommand
import core.Contexts.{Context, ContextBase}
import util.DotClass
import reporting._
import scala.util.control.NonFatal

abstract class Driver extends DotClass {

  val prompt = "\ndotc> "

  protected def newCompiler(): Compiler

  protected def emptyReporter: Reporter = new StoreReporter(null)

  protected def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter =
    if (fileNames.nonEmpty)
      try {
        val run = compiler.newRun
        run.compile(fileNames)
        run.printSummary()
      }
      catch {
        case ex: FatalError  =>
          ctx.error(ex.getMessage) // signals that we should fail compilation.
          ctx.reporter
      }
    else emptyReporter

  protected def initCtx = (new ContextBase).initialCtx

  protected def sourcesRequired = true

  def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)
    ctx.setSettings(summary.sstate)
    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    (fileNames, ctx)
  }


  /** Principal entry point to the compiler.
   *  Creates a new compiler instance and run it with arguments `args`.
   *
   *  The optional arguments of this method all have `null` as their default
   *  value, this makes it easier to call this method by reflection or from Java.
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param reporter   Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  final def process(args: Array[String], reporter: Reporter = null,
    callback: CompilerCallback = null): Reporter = {
    val ctx = initCtx.fresh
    if (reporter != null)
      ctx.setReporter(reporter)
    if (callback != null)
      ctx.setCompilerCallback(callback)
    process(args, ctx)
  }

  /** Entry point to the compiler with no optional arguments.
   *
   *  This overload is provided for compatibility reasons: the
   *  `RawCompiler` of sbt expects this method to exist and calls
   *  it using reflection. Keeping it means that we can change
   *  the other overloads without worrying about breaking compatibility
   *  with sbt.
   */
  final def process(args: Array[String]): Reporter =
    process(args, null, null)

  /** Entry point to the compiler using a custom `Context`.
   *
   *  In most cases, you do not need a custom `Context` and should
   *  instead use one of the other overloads of `process`. However,
   *  the other overloads cannot be overriden, instead you
   *  should override this one which they call internally.
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param rootCtx    The root Context to use.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  def process(args: Array[String], rootCtx: Context): Reporter = {
    val (fileNames, ctx) = setup(args, rootCtx)
    doCompile(newCompiler(), fileNames)(ctx)
  }

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }
}

class FatalError(msg: String) extends Exception

