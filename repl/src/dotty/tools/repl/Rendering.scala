package dotty.tools
package repl

import java.lang.ClassLoader

import scala.util.control.NonFatal

import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Symbols.Symbol
// FIXME: perhaps `AbstractFileClassLoader` should be moved up a pkg
import dotc.repl.AbstractFileClassLoader

private[repl] object Rendering {

  private var parentClassLoader: Option[ClassLoader] = None

  private def classLoader(compiler: ReplCompiler)(implicit ctx: Context) = {
    import java.net.{URL, URLClassLoader}

    /** the compiler's classpath, as URL's */
    val compilerClasspath: Seq[URL] = ctx.platform.classPath(ctx).asURLs

    def parent = new URLClassLoader(compilerClasspath.toArray,
                                    classOf[ReplDriver].getClassLoader)

    val newClsLoader = new AbstractFileClassLoader(compiler.directory,
                                parentClassLoader.getOrElse(parent))

    Thread.currentThread.setContextClassLoader(newClsLoader)
    parentClassLoader = Some(newClsLoader)
    newClsLoader
  }


  /** Load the value of the symbol using reflection
   *
   *  Calling this method evaluates the expression using reflection
   */
  private[this] def valueOf(sym: Symbol, compiler: ReplCompiler)(implicit ctx: Context): Option[String] = {
    val defn = ctx.definitions
    val objectName = sym.owner.fullName.encode.toString.dropRight(1) // gotta drop the '$'
    val resObj: Class[_] = Class.forName(objectName, true, classLoader(compiler))

    val res =
      resObj
      .getDeclaredMethods.find(_.getName == sym.name.toString + "Show").get
      .invoke(null).toString

    val shellPrefix = "ReplSession$"
    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else if (res.startsWith(shellPrefix))
      Some(res.drop(shellPrefix.length).dropWhile(c => c.isDigit || c == '$'))
    else
      Some(res)
  }

  /** Render method definition result */
  def renderMethod(d: Denotation)(implicit ctx: Context): String =
    d.symbol.showUser

  /** Render value definition result */
  def renderVal(d: Denotation, compiler: ReplCompiler)(implicit ctx: Context): Option[String] = {
    val dcl = d.symbol.showUser
    val resultValue =
      if (d.symbol.is(Flags.Lazy)) Some("<lazy>")
      else valueOf(d.symbol, compiler)

    resultValue.map(value => s"$dcl = $value")
  }
}
