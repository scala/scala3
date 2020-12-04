package scala.quoted
package staging

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, FreshContext}
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.util.ClasspathFromClassloader
import scala.quoted._
import scala.quoted.staging.Toolbox
import java.io.File
import scala.annotation.tailrec

/** Driver to compile quoted code
 *
 * @param appClassloader classloader of the application that generated the quotes
 */
private class QuoteDriver(appClassloader: ClassLoader) extends Driver:
  import tpd._

  private[this] val contextBase: ContextBase = new ContextBase

  def run[T](exprBuilder: Quotes => Expr[T], settings: Toolbox.Settings): T =
    val outDir: AbstractFile =
      settings.outDir match
        case Some(out) =>
          val dir = Directory(out)
          dir.createDirectory()
          new PlainDirectory(Directory(out))
        case None =>
          new VirtualDirectory("<quote compilation output>")
    end outDir

    val (_, ctx0: Context) = setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)
    val ctx = setToolboxSettings(ctx0.fresh.setSetting(ctx0.settings.outputDir, outDir), settings)

    new QuoteCompiler().newRun(ctx).compileExpr(exprBuilder) match
      case Right(value) =>
        value.asInstanceOf[T]

      case Left(classname) =>
        assert(!ctx.reporter.hasErrors)

        val classLoader = new AbstractFileClassLoader(outDir, appClassloader)

        val clazz = classLoader.loadClass(classname)
        val method = clazz.getMethod("apply")
        val inst = clazz.getConstructor().newInstance()

        method.invoke(inst).asInstanceOf[T]
    end match

  end run

  override def initCtx: Context =
    val ictx = contextBase.initialCtx
    ictx.settings.classpath.update(ClasspathFromClassloader(appClassloader))(using ictx)
    ictx

  private def setToolboxSettings(ctx: FreshContext, settings: Toolbox.Settings): ctx.type =
    // An error in the generated code is a bug in the compiler
    // Setting the throwing reporter however will report any exception
    ctx.setReporter(new ThrowingReporter(ctx.reporter))

end QuoteDriver
