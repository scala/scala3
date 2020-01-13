package scala.quoted
package staging

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, FreshContext}
import dotty.tools.dotc.tastyreflect.ReflectionImpl
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.reporting._
import scala.quoted._
import scala.quoted.staging.Toolbox
import java.net.URLClassLoader
import java.nio.file.Paths
import java.io.File
import scala.annotation.tailrec

/** Driver to compile quoted code
 *
 * @param appClassloader classloader of the application that generated the quotes
 */
private class QuoteDriver(appClassloader: ClassLoader) extends Driver {
  import tpd._

  private[this] val contextBase: ContextBase = new ContextBase

  def run[T](exprBuilder: QuoteContext => Expr[T], settings: Toolbox.Settings): T = {
    val outDir: AbstractFile = settings.outDir match {
      case Some(out) =>
        val dir = Directory(out)
        dir.createDirectory()
        new PlainDirectory(Directory(out))
      case None =>
        new VirtualDirectory("<quote compilation output>")
    }

    val (_, ctx0: Context) = setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)
    val ctx = setToolboxSettings(ctx0.fresh.setSetting(ctx0.settings.outputDir, outDir), settings)

    new QuoteCompiler().newRun(ctx).compileExpr(exprBuilder) match {
      case Right(value) =>
        value.asInstanceOf[T]

      case Left(classname) =>
        assert(!ctx.reporter.hasErrors)

        val classLoader = new AbstractFileClassLoader(outDir, appClassloader)

        val clazz = classLoader.loadClass(classname)
        val method = clazz.getMethod("apply")
        val inst = clazz.getConstructor().newInstance()

        method.invoke(inst).asInstanceOf[T]
    }
  }

  override def initCtx: Context = {
    val ictx = contextBase.initialCtx
    ictx.settings.classpath.update(classpathFromClassloader(appClassloader))(ictx)
    ictx
  }

  private def setToolboxSettings(ctx: FreshContext, settings: Toolbox.Settings): ctx.type = {
    ctx.setSetting(ctx.settings.YshowRawQuoteTrees, settings.showRawTree)
    // An error in the generated code is a bug in the compiler
    // Setting the throwing reporter however will report any exception
    ctx.setReporter(new ThrowingReporter(ctx.reporter))
  }

  /** Attempt to recreate a classpath from a classloader.
   *
   *  BEWARE: with exotic enough classloaders, this may not work at all or do
   *  the wrong thing.
   */
  private def classpathFromClassloader(cl: ClassLoader): String = {
    val classpathBuff = List.newBuilder[String]
    def collectClassLoaderPaths(cl: ClassLoader): Unit = {
      if (cl != null) {
        cl match {
          case cl: URLClassLoader =>
            // This is wrong if we're in a subclass of URLClassLoader
            // that filters loading classes from its parent ¯\_(ツ)_/¯
            collectClassLoaderPaths(cl.getParent)
            // Parent classloaders are searched before their child, so the part of
            // the classpath coming from the child is added at the _end_ of the
            // classpath.
            classpathBuff ++=
              cl.getURLs.iterator.map(url => Paths.get(url.toURI).toAbsolutePath.toString)
          case _ =>
            // HACK: We can't just collect the classpath from arbitrary parent
            // classloaders since the current classloader might intentionally
            // filter loading classes from its parent (for example
            // BootFilteredLoader in the sbt launcher does this and we really
            // don't want to include the scala-library that sbt depends on
            // here), but we do need to look at the parent of the REPL
            // classloader, so we special case it. We can't do this using a type
            // test since the REPL classloader class itself is normally loaded
            // with a different classloader.
            if (cl.getClass.getName == classOf[AbstractFileClassLoader].getName)
              collectClassLoaderPaths(cl.getParent)
        }
      }
    }
    collectClassLoaderPaths(cl)
    classpathBuff.result().mkString(java.io.File.pathSeparator)
  }
}

