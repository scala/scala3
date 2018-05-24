package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.printing.DecompilerPrinter

import scala.quoted.{Expr, Type}

import java.net.URLClassLoader

import Toolbox.{Settings, Run, Show}

class QuoteDriver extends Driver {
  import tpd._

  def run[T](expr: Expr[T], settings: Settings[Run]): T = {
    val (_, ctx: Context) = setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)

    val outDir: AbstractFile = settings.outDir match {
      case Some(out) =>
        val dir = Directory(out)
        dir.createDirectory()
        new PlainDirectory(Directory(out))
      case None =>
        new VirtualDirectory("(memory)", None)
    }

    val driver = new QuoteCompiler(outDir)
    driver.newRun(ctx).compileExpr(expr)

    val classLoader = new AbstractFileClassLoader(outDir, this.getClass.getClassLoader)

    val clazz = classLoader.loadClass(driver.outputClassName.toString)
    val method = clazz.getMethod("apply")
    val instance = clazz.newInstance()

    method.invoke(instance).asInstanceOf[T]
  }

  def show(expr: Expr[_], settings: Settings[Show]): String = {
    def show(tree: Tree, ctx: Context): String = {
      val printer = new DecompilerPrinter(ctx)
      val pageWidth = ctx.settings.pageWidth.value(ctx)
      val tree1 = if (settings.rawTree) tree else (new TreeCleaner).transform(tree)(ctx)
      printer.toText(tree1).mkString(pageWidth, false)
    }
    withTree(expr, show, settings)
  }

  def withTree[T](expr: Expr[_], f: (Tree, Context) => T, settings: Settings[_]): T = {
    val (_, ctx: Context) = setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)

    var output: Option[T] = None
    def registerTree(tree: tpd.Tree)(ctx: Context): Unit = {
      assert(output.isEmpty)
      output = Some(f(tree, ctx))
    }
    new QuoteDecompiler(registerTree).newRun(ctx).compileExpr(expr)
    output.getOrElse(throw new Exception("Could not extract " + expr))
  }

  def withTypeTree[T](tpe: Type[_], f: (TypTree, Context) => T, settings: Settings[_]): T = {
    val (_, ctx: Context) = setup(settings.compilerArgs.toArray :+ "dummy.scala", initCtx.fresh)

    var output: Option[T] = None
    def registerTree(tree: tpd.Tree)(ctx: Context): Unit = {
      assert(output.isEmpty)
      output = Some(f(tree.asInstanceOf[TypTree], ctx))
    }
    new QuoteDecompiler(registerTree).newRun(ctx).compileType(tpe)
    output.getOrElse(throw new Exception("Could not extract " + tpe))
  }

  override def initCtx: Context = {
    val ictx = super.initCtx.fresh
    var classpath = System.getProperty("java.class.path")
    this.getClass.getClassLoader match {
      case cl: URLClassLoader =>
        // Loads the classes loaded by this class loader
        // When executing `run` or `test` in sbt the classpath is not in the property java.class.path
        val newClasspath = cl.getURLs.map(_.getFile())
        classpath = newClasspath.mkString("", ":", if (classpath == "") "" else ":" + classpath)
      case _ =>
    }
    ictx.settings.classpath.update(classpath)(ictx)
    ictx
  }

}
