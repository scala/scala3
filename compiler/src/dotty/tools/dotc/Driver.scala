package dotty.tools
package dotc

import dotty.tools.FatalError
import config.CompilerCommand
import core.Comments.{ContextDoc, ContextDocstrings}
import core.Contexts.{Context, ContextBase}
import core.Mode
import util.DotClass
import reporting._
import scala.util.control.NonFatal
import fromtasty.TASTYCompiler
import io.VirtualDirectory

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

/** Run the Dotty compiler.
 *
 *  Extending this class lets you customize many aspect of the compilation
 *  process, but in most cases you only need to call [[process]] on the
 *  existing object [[Main]].
 */
class Driver extends DotClass {

  protected def newCompiler(implicit ctx: Context): Compiler =
    if (ctx.settings.fromTasty.value) new TASTYCompiler
    else new Compiler

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
    else ctx.reporter

  protected def initCtx = (new ContextBase).initialCtx

  protected def sourcesRequired = true

  def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)
    ctx.setSettings(summary.sstate)

    if (!ctx.settings.YdropComments.value(ctx) || ctx.mode.is(Mode.ReadComments)) {
      ctx.setProperty(ContextDoc, new ContextDocstrings)
    }

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    (fileNames, ctx)
  }

  /** Entry point to the compiler that can be conveniently used with Java reflection.
   *
   *  This entry point can easily be used without depending on the `dotty` package,
   *  you only need to depend on `dotty-interfaces` and call this method using
   *  reflection. This allows you to write code that will work against multiple
   *  versions of dotty without recompilation.
   *
   *  The trade-off is that you can only pass a SimpleReporter to this method
   *  and not a normal Reporter which is more powerful.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/test/test/InterfaceEntryPointTest.scala]]
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param simple     Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return
   */
  final def process(args: Array[String], simple: interfaces.SimpleReporter,
    callback: interfaces.CompilerCallback): interfaces.ReporterResult = {
    val reporter = if (simple == null) null else Reporter.fromSimpleReporter(simple)
    process(args, reporter, callback)
  }

  /** Principal entry point to the compiler.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/test/test/OtherEntryPointsTest.scala]]
   *  in method `runCompiler`
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
    callback: interfaces.CompilerCallback = null): Reporter = {
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
    process(args, null: Reporter, null: interfaces.CompilerCallback)

  /** Entry point to the compiler using a custom `Context`.
   *
   *  In most cases, you do not need a custom `Context` and should
   *  instead use one of the other overloads of `process`. However,
   *  the other overloads cannot be overriden, instead you
   *  should override this one which they call internally.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/test/test/OtherEntryPointsTest.scala]]
   *  in method `runCompilerWithContext`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param rootCtx    The root Context to use.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  def process(args: Array[String], rootCtx: Context): Reporter = {
    def compile(files: List[String], ctx: Context) = doCompile(newCompiler(ctx), files)(ctx)

    val (fileNames, ctx) = setup(args, rootCtx)
    val parallelism = {
      val p = ctx.settings.parallelism.value(ctx)
      if (p != 1 && (
          ctx.settings.YemitTastyInClass.value(ctx) ||
          ctx.settings.YtestPickler.value(ctx) ||
          ctx.settings.fromTasty.value(ctx))) {
        ctx.warning("Parallel compilation disabled due to incompatible setting.")
        1
      }
      else if (p == 0)
        Runtime.getRuntime().availableProcessors
      else
        p
    }
    if (parallelism == 1)
      compile(fileNames, ctx)
    else {
      val tastyOutlinePath = new VirtualDirectory("<tasty outline>")

      // First pass: generate .tasty outline files
      val firstPassCtx = ctx.fresh
        .setSetting(ctx.settings.outputDir, tastyOutlinePath)
        .setSetting(ctx.settings.YemitTastyOutline, true)
        .setSbtCallback(null) // Do not run the sbt-specific phases in this pass
        .setCompilerCallback(null) // TODO: Change the CompilerCallback API to handle two-pass compilation?

      compile(fileNames, firstPassCtx)

      val scalaFileNames = fileNames.filterNot(_.endsWith(".java"))
      if (!firstPassCtx.reporter.hasErrors && scalaFileNames.nonEmpty) {
        // Second pass: split the list of files into $parallelism groups,
        // compile each group independently.


        val maxGroupSize = Math.ceil(scalaFileNames.length.toDouble / parallelism).toInt
        val fileGroups = scalaFileNames.grouped(maxGroupSize).toList
        val compilers = fileGroups.length

        // Needed until https://github.com/sbt/zinc/pull/410 is merged.
        val synchronizedSbtCallback =
          if (rootCtx.sbtCallback != null)
            new sbt.SynchronizedAnalysisCallback(rootCtx.sbtCallback)
          else
            null

        def secondPassCtx = {
          // TODO: figure out which parts of rootCtx we can safely reuse exactly.
          val baseCtx = initCtx.fresh
            .setSettings(rootCtx.settingsState)
            .setReporter(new StoreReporter(rootCtx.reporter))
            .setSbtCallback(synchronizedSbtCallback)
            .setCompilerCallback(rootCtx.compilerCallback)

          val (_, ctx) = setup(args, baseCtx)
          ctx.fresh.setSetting(ctx.settings.priorityclasspath, tastyOutlinePath)
        }

        val executor = Executors.newFixedThreadPool(compilers)
        implicit val ec = ExecutionContext.fromExecutor(executor)

        val futureReporters = Future.sequence(fileGroups.map(fileGroup => Future {
          // println("#Compiling: " + fileGroup.mkString(" "))
          val reporter = compile(fileGroup, secondPassCtx)
          // println("#Done: " + fileGroup.mkString(" "))
          reporter
        })).andThen {
          case Success(reporters) =>
            reporters.foreach(_.flush()(firstPassCtx))
          case Failure(ex) =>
            ex.printStackTrace
            firstPassCtx.error(s"Exception during parallel compilation: ${ex.getMessage}")
        }
        Await.ready(futureReporters, Duration.Inf)
        executor.shutdown()
      }
      firstPassCtx.reporter
    }
  }

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }
}
