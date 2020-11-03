package dotty.tools.dotc

import java.nio.file.{Files, Paths}

import dotty.tools.FatalError
import config.CompilerCommand
import core.Comments.{ContextDoc, ContextDocstrings}
import core.Contexts._
import core.{MacroClassLoader, Mode, TypeError}
import core.StdNames.nme
import dotty.tools.dotc.ast.Positioned
import dotty.tools.io.File
import reporting._
import core.Decorators._
import config.Feature

import scala.util.control.NonFatal
import fromtasty.{TASTYCompiler, TastyFileUtil}

/** Run the Dotty compiler.
 *
 *  Extending this class lets you customize many aspect of the compilation
 *  process, but in most cases you only need to call [[process]] on the
 *  existing object [[Main]].
 */
class Driver {

  protected def newCompiler(using Context): Compiler =
    if (ctx.settings.fromTasty.value) new TASTYCompiler
    else new Compiler

  protected def emptyReporter: Reporter = new StoreReporter(null)

  protected def doCompile(compiler: Compiler, fileNames: List[String])(using Context): Reporter =
    if (fileNames.nonEmpty)
      try
        val run = compiler.newRun
        run.compile(fileNames)

        def finish(run: Run)(using Context): Unit =
          run.printSummary()
          if !ctx.reporter.errorsReported && run.suspendedUnits.nonEmpty then
            val suspendedUnits = run.suspendedUnits.toList
            if (ctx.settings.XprintSuspension.value)
              report.echo(i"compiling suspended $suspendedUnits%, %")
            val run1 = compiler.newRun
            for unit <- suspendedUnits do unit.suspended = false
            run1.compileUnits(suspendedUnits)
            finish(run1)(using MacroClassLoader.init(ctx.fresh))

        finish(run)
      catch
        case ex: FatalError  =>
          report.error(ex.getMessage) // signals that we should fail compilation.
        case ex: TypeError =>
          println(s"${ex.toMessage} while compiling ${fileNames.mkString(", ")}")
          throw ex
        case ex: Throwable =>
          println(s"$ex while compiling ${fileNames.mkString(", ")}")
          throw ex
    ctx.reporter
  end doCompile

  protected def initCtx: Context = (new ContextBase).initialCtx

  protected def sourcesRequired: Boolean = true

  def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ictx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(using ictx)
    ictx.setSettings(summary.sstate)
    MacroClassLoader.init(ictx)
    Positioned.init(using ictx)

    inContext(ictx) {
      if !ctx.settings.YdropComments.value || ctx.mode.is(Mode.ReadComments) then
        ictx.setProperty(ContextDoc, new ContextDocstrings)
      if Feature.enabledBySetting(nme.Scala2Compat) then
        report.warning("-language:Scala2Compat will go away; use -source 3.0-migration instead")
      val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)
      fromTastySetup(fileNames, ctx)
    }
  }

  /** Setup extra classpath and figure out class names for tasty file inputs */
  protected def fromTastySetup(fileNames0: List[String], ctx0: Context): (List[String], Context) =
    given Context = ctx0
    if (ctx0.settings.fromTasty.value) {
      val fromTastyIgnoreList = ctx0.settings.YfromTastyIgnoreList.value.toSet
      // Resolve classpath and class names of tasty files
      val (classPaths, classNames) = fileNames0.flatMap { name =>
        val path = Paths.get(name)
        if !name.endsWith(".jar") && !name.endsWith(".tasty") then // is class name
          ("", name) :: Nil // TODO remove this case. We cannot rely on an expected tasty file beeing loaded.
        else if !Files.exists(path) then
          report.error(s"File does not exist: $name")
          Nil
        else if name.endsWith(".jar") then
          new dotty.tools.io.Jar(File(name)).toList.collect {
            case e if e.getName.endsWith(".tasty") && !fromTastyIgnoreList(e.getName) =>
              (name, e.getName.stripSuffix(".tasty").replace("/", "."))
          }
        else
          assert(name.endsWith(".tasty"))
          TastyFileUtil.getClassName(path) match
            case Some(res) => res :: Nil
            case _ =>
              report.error(s"Could not load classname from: $name")
              Nil
      }.unzip
      val ctx1 = ctx0.fresh
      val classPaths1 = classPaths.distinct.filter(_ != "")
      val fullClassPath = (classPaths1 :+ ctx1.settings.classpath.value(using ctx1)).mkString(java.io.File.pathSeparator)
      ctx1.setSetting(ctx1.settings.classpath, fullClassPath)
      (classNames, ctx1)
    }
    else (fileNames0, ctx0)

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
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/InterfaceEntryPointTest.scala]]
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
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
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
    val compileCtx = initCtx.fresh
    if (reporter != null)
      compileCtx.setReporter(reporter)
    if (callback != null)
      compileCtx.setCompilerCallback(callback)
    process(args, compileCtx)
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
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
   *  in method `runCompilerWithContext`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param rootCtx    The root Context to use.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  def process(args: Array[String], rootCtx: Context): Reporter = {
    val (fileNames, compileCtx) = setup(args, rootCtx)
    doCompile(newCompiler(using compileCtx), fileNames)(using compileCtx)
  }

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }
}
