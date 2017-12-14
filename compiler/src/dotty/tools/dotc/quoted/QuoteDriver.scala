package dotty.tools.dotc.quoted

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.io.VirtualDirectory

import dotty.tools.repl.AbstractFileClassLoader

import scala.quoted.Expr

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets

class QuoteDriver extends Driver {

  def run[T](expr: Expr[T]): T = {
    val ctx: Context = initCtx.fresh
    // TODO enable optimisation?
    // ctx.settings.optimise.update(true)(ctx)

    val outDir = new VirtualDirectory("(memory)", None)

    new ExprCompiler(outDir).newRun(ctx).compileExpr(expr)

    val classLoader = new AbstractFileClassLoader(outDir, this.getClass.getClassLoader)

    val clazz = classLoader.loadClass(nme.QUOTE.toString)
    val method = clazz.getMethod("apply")
    val instance = clazz.newInstance()

    method.invoke(instance).asInstanceOf[T]
  }

  def show(expr: Expr[_]): String = {
    val ctx: Context = initCtx.fresh
    ctx.settings.color.update("never")(ctx) // TODO support colored show
    val baos = new ByteArrayOutputStream
    var ps: PrintStream = null
    try {
      ps = new PrintStream(baos, true, "utf-8")

      new ExprDecompiler(ps).newRun(ctx).compileExpr(expr)

      new String(baos.toByteArray, StandardCharsets.UTF_8)
    }
    finally if (ps != null) ps.close()
  }

  override def initCtx: Context = {
    val ictx = super.initCtx.fresh
    val compilerClasspath = System.getProperty("dotty.tools.dotc.classpath")
    assert(compilerClasspath ne null, "System property `dotty.tools.dotc.classpath` is not set.")
    val classpath = System.getProperty("java.class.path")
    val scalaLib = classpath.split(":").filter(_.contains("scala-library")).mkString(":")
    ictx.settings.classpath.update(compilerClasspath + ":" + scalaLib)(ictx)
    ictx
  }

}
