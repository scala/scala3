package scala.quoted
package staging

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, FreshContext}
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.util.ClasspathFromClassloader
import scala.quoted._
import scala.quoted.staging.Compiler
import java.io.File
import scala.annotation.tailrec

/** Driver to compile quoted code
 *
 * @param appClassloader classloader of the application that generated the quotes
 */
private class QuoteDriver(appClassloader: ClassLoader) extends Driver:
  import tpd._

  private val contextBase: ContextBase = new ContextBase

  def run[T](exprBuilder: Quotes => Expr[T], settings: Compiler.Settings): T =
    val outDir: AbstractFile =
      settings.outDir match
        case Some(out) =>
          val dir = Directory(out)
          dir.createDirectory()
          new PlainDirectory(Directory(out))
        case None =>
          new VirtualDirectory("<quote compilation output>")
    end outDir

    val ctx = {
      val ctx0 = QuotesCache.init(initCtx.fresh)
      val ctx1 = setup(settings.compilerArgs.toArray :+ "dummy.scala", ctx0).get._2
      setCompilerSettings(ctx1.fresh.setSetting(ctx1.settings.outputDir, outDir), settings)
    }

    val compiledExpr = 
      try
        new QuoteCompiler().newRun(ctx).compileExpr(exprBuilder)
      catch case ex: dotty.tools.FatalError =>
        val enrichedMessage =
          s"""An unhandled exception was thrown in the staging compiler.
            |This might be caused by using an incorrect classloader
            |when creating the `staging.Compiler` instance with `staging.Compiler.make`.
            |For details, please refer to the documentation.
            |For non-enriched exceptions, compile with -Yno-enrich-error-messages.""".stripMargin
        if ctx.settings.YnoEnrichErrorMessages.value(using ctx) then throw ex
        else throw new Exception(enrichedMessage, ex)
    
    compiledExpr match
      case Right(value) =>
        value.asInstanceOf[T]

      case Left(classname) =>
        assert(!ctx.reporter.hasErrors)

        val classLoader = new AbstractFileClassLoader(outDir, appClassloader)

        val clazz = classLoader.loadClass(classname)
        val method = clazz.getMethod("apply")
        val inst = clazz.getConstructor().newInstance()

        try method.invoke(inst).asInstanceOf[T]
        catch case ex: java.lang.reflect.InvocationTargetException =>
          ex.getCause match
            case _: java.lang.NoClassDefFoundError =>
              throw new Exception(
                s"""`scala.quoted.staging.run` failed to load a class.
                   |The classloader used for the `staging.Compiler` instance might not be the correct one.
                   |Make sure that this classloader is the one that loaded the missing class.
                   |Note that the classloader that loads the standard library might not be the same as
                   |the one that loaded the application classes.""".stripMargin,
                ex)

            case _ => throw ex
    end match

  end run

  override def initCtx: Context =
    val ictx = contextBase.initialCtx
    ictx.settings.classpath.update(ClasspathFromClassloader(appClassloader))(using ictx)
    ictx

  private def setCompilerSettings(ctx: FreshContext, settings: Compiler.Settings): ctx.type =
    // An error in the generated code is a bug in the compiler
    // Setting the throwing reporter however will report any exception
    ctx.setReporter(new ThrowingReporter(ctx.reporter))

end QuoteDriver
