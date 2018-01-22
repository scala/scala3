package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.printing.DecompilerPrinter

import scala.quoted.Expr

class QuoteDriver extends Driver {
  import tpd._

  def run[T](expr: Expr[T], settings: Runners.RunSettings): T = {
    val ctx: Context = initCtx.fresh
    ctx.settings.optimise.update(settings.optimise)(ctx)

    val outDir: AbstractFile = settings.outDir match {
      case Some(out) =>
        val dir = Directory(out)
        dir.createDirectory()
        new PlainDirectory(Directory(out))
      case None =>
        new VirtualDirectory("(memory)", None)
    }

    val driver = new ExprCompiler(outDir)
    driver.newRun(ctx).compileExpr(expr)

    val classLoader = new AbstractFileClassLoader(outDir, this.getClass.getClassLoader)

    val clazz = classLoader.loadClass(driver.outputClassName.toString)
    val method = clazz.getMethod("apply")
    val instance = clazz.newInstance()

    method.invoke(instance).asInstanceOf[T]
  }

  def show(expr: Expr[_]): String = {
    def show(tree: Tree, ctx: Context): String = {
      val printer = new DecompilerPrinter(ctx)
      val pageWidth = ctx.settings.pageWidth.value(ctx)
      printer.toText(tree).mkString(pageWidth, false)
    }
    withTree(expr, show)
  }

  def withTree[T](expr: Expr[_], f: (Tree, Context) => T): T = {
    val ctx: Context = initCtx.fresh
    ctx.settings.color.update("never")(ctx) // TODO support colored show
    var output: Option[T] = None
    def registerTree(tree: tpd.Tree)(ctx: Context): Unit = {
      assert(output.isEmpty)
      output = Some(f(tree, ctx))
    }
    new ExprDecompiler(registerTree).newRun(ctx).compileExpr(expr)
    output.getOrElse(throw new Exception("Could not extact " + expr))
  }

  override def initCtx: Context = {
    val ictx = super.initCtx.fresh
    val classpath = System.getProperty("java.class.path")
    ictx.settings.classpath.update(classpath)(ictx)
    ictx
  }

}
