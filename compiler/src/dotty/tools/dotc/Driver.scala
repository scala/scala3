package dotty.tools.dotc

import java.nio.file.{Files, Paths}

import dotty.tools.FatalError
import config.CompilerCommand
import core.Comments.{ContextDoc, ContextDocstrings}
import core.Contexts.{Context, ContextBase}
import core.{MacroClassLoader, Mode, TypeError}
import dotty.tools.dotc.ast.Positioned
import reporting._

import scala.util.control.NonFatal
import fromtasty.{TASTYCompiler, TastyFileUtil}

/** Run the Dotty compiler.
 *
 *  Extending this class lets you customize many aspect of the compilation
 *  process, but in most cases you only need to call [[process]] on the
 *  existing object [[Main]].
 */
class Driver {

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
        case ex: TypeError =>
          println(s"${ex.toMessage} while compiling ${fileNames.mkString(", ")}")
          throw ex
        case ex: Throwable =>
          println(s"$ex while compiling ${fileNames.mkString(", ")}")
          throw ex
      }
    else ctx.reporter

  protected def initCtx: Context = (new ContextBase).initialCtx

  protected def sourcesRequired: Boolean = true

  def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)
    ctx.setSettings(summary.sstate)
    MacroClassLoader.init(ctx)
    Positioned.updateDebugPos(ctx)

    if (!ctx.settings.YdropComments.value(ctx) || ctx.mode.is(Mode.ReadComments)) {
      ctx.setProperty(ContextDoc, new ContextDocstrings)
    }

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    fromTastySetup(fileNames, ctx)
  }

  /** Setup extra classpath and figure out class names for tasty file inputs */
  protected def fromTastySetup(fileNames0: List[String], ctx0: Context): (List[String], Context) = {
    if (ctx0.settings.fromTasty.value(ctx0)) {
      // Resolve classpath and class names of tasty files
      val (classPaths, classNames) = fileNames0.map { name =>
        val path = Paths.get(name)
        if (!name.endsWith(".tasty")) ("", name)
        else if (Files.exists(path)) {
          TastyFileUtil.getClassName(path) match {
            case Some(res) => res
            case _ =>
              ctx0.error(s"Could not load classname from $name.")
              ("", name)
          }
        } else {
          ctx0.error(s"File $name does not exist.")
          ("", name)
        }
      }.unzip
      val ctx1 = ctx0.fresh
      val classPaths1 = classPaths.distinct.filter(_ != "")
      val fullClassPath = (classPaths1 :+ ctx1.settings.classpath.value(ctx1)).mkString(java.io.File.pathSeparator)
      ctx1.setSetting(ctx1.settings.classpath, fullClassPath)
      (classNames, ctx1)
    } else (fileNames0, ctx0)
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
   *  the other overloads cannot be overridden, instead you
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
    val (fileNames, ctx) = setup(args, rootCtx)
    doCompile(newCompiler(ctx), fileNames)(ctx)
  }

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }
}
