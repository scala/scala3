package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, FreshContext}
import dotty.tools.dotc.tastyreflect.ReflectionImpl
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.reporting._
import scala.quoted.{Expr, Type}
import scala.quoted.Toolbox
import java.net.URLClassLoader

/** Driver to compile quoted code
  *
  * @param appClassloader classloader of the application that generated the quotes
  */
class QuoteDriver(appClassloader: ClassLoader) extends Driver {
  import tpd._

  private[this] val contextBase: ContextBase = new ContextBase

  def run[T](expr: Expr[T], settings: Toolbox.Settings): T = {
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

    val driver = new QuoteCompiler
    driver.newRun(ctx).compileExpr(expr)

    assert(!ctx.reporter.hasErrors)

    val classLoader = new AbstractFileClassLoader(outDir, appClassloader)

    val clazz = classLoader.loadClass(driver.outputClassName.toString)
    val method = clazz.getMethod("apply")
    val inst = clazz.getConstructor().newInstance()

    method.invoke(inst).asInstanceOf[T]
  }

  private def doShow(tree: Tree, ctx: Context): String = {
    implicit val c: Context = ctx
    val tree1 =
      if (ctx.settings.YshowRawQuoteTrees.value) tree
      else (new TreeCleaner).transform(tree)
    ReflectionImpl.showTree(tree1)
  }

  def show(expr: Expr[_], settings: Toolbox.Settings): String =
    withTree(expr, doShow, settings)

  def show(tpe: Type[_], settings: Toolbox.Settings): String =
    withTypeTree(tpe, doShow, settings)

  def withTree[T](expr: Expr[_], f: (Tree, Context) => T, settings: Toolbox.Settings): T = {
    val ctx = setToolboxSettings(setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)._2.fresh, settings)

    var output: Option[T] = None
    def registerTree(tree: tpd.Tree)(ctx: Context): Unit = {
      assert(output.isEmpty)
      output = Some(f(tree, ctx))
    }
    new QuoteDecompiler(registerTree).newRun(ctx).compileExpr(expr)
    output.getOrElse(throw new Exception("Could not extract " + expr))
  }

  def withTypeTree[T](tpe: Type[_], f: (TypTree, Context) => T, settings: Toolbox.Settings): T = {
    val ctx = setToolboxSettings(setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)._2.fresh, settings)

    var output: Option[T] = None
    def registerTree(tree: tpd.Tree)(ctx: Context): Unit = {
      assert(output.isEmpty)
      output = Some(f(tree.asInstanceOf[TypTree], ctx))
    }
    new QuoteDecompiler(registerTree).newRun(ctx).compileType(tpe)
    output.getOrElse(throw new Exception("Could not extract " + tpe))
  }

  override def initCtx: Context = {
    val ictx = contextBase.initialCtx
    ictx.settings.classpath.update(QuoteDriver.currentClasspath(appClassloader))(ictx)
    ictx
  }

  private def setToolboxSettings(ctx: FreshContext, settings: Toolbox.Settings): ctx.type = {
    ctx.setSetting(ctx.settings.color, if (settings.color) "always" else "never")
    ctx.setSetting(ctx.settings.YshowRawQuoteTrees, settings.showRawTree)
    // An error in the generated code is a bug in the compiler
    // Setting the throwing reporter however will report any exception
    ctx.setReporter(new ThrowingReporter(ctx.reporter))
  }
}

object QuoteDriver {

  def currentClasspath(cl: ClassLoader): String = {
    val classpath0 = System.getProperty("java.class.path")
    cl match {
      case cl: URLClassLoader =>
        // Loads the classes loaded by this class loader
        // When executing `run` or `test` in sbt the classpath is not in the property java.class.path
        import java.nio.file.Paths
        val newClasspath = cl.getURLs.map(url => Paths.get(url.toURI).toString)
        newClasspath.mkString("", java.io.File.pathSeparator, if (classpath0 == "") "" else java.io.File.pathSeparator + classpath0)
      case _ => classpath0
    }
  }

}
