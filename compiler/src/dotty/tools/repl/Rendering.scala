package dotty.tools
package repl

import java.io.{PrintWriter, StringWriter}
import java.lang.{ClassLoader, ExceptionInInitializerError}
import java.lang.reflect.InvocationTargetException

import scala.util.control.NonFatal
import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Symbols.Symbol
import dotc.core.StdNames.str

import scala.util.{Failure, Success, Try}

/** This rendering object uses `ClassLoader`s to accomplish crossing the 4th
 *  wall (i.e. fetching back values from the compiled class files put into a
 *  specific class loader capable of loading from memory) and rendering them.
 *
 *  @pre this object should be paired with a compiler session, i.e. when
 *       `ReplDriver#resetToInitial` is called, the accompanying instance of
 *       `Rendering` is no longer valid.
 */
private[repl] class Rendering(compiler: ReplCompiler,
                              parentClassLoader: Option[ClassLoader] = None) {

  private[this] var myClassLoader: ClassLoader = _

  /** Class loader used to load compiled code */
  private[this] def classLoader()(implicit ctx: Context) =
    if (myClassLoader != null) myClassLoader
    else {
      val parent = parentClassLoader.getOrElse {
        // the compiler's classpath, as URL's
        val compilerClasspath = ctx.platform.classPath(ctx).asURLs
        new java.net.URLClassLoader(compilerClasspath.toArray, classOf[ReplDriver].getClassLoader)
      }

      myClassLoader = new AbstractFileClassLoader(compiler.directory, parent)
      // Set the current Java "context" class loader to this rendering class loader
      Thread.currentThread.setContextClassLoader(myClassLoader)
      myClassLoader
    }

  /** Load the value of the symbol using reflection
   *
   *  Calling this method evaluates the expression using reflection
   */
  private[this] def valueOf(sym: Symbol)(implicit ctx: Context): Option[String] = {
    val defn = ctx.definitions
    val objectName = sym.owner.fullName.encode.toString.dropRight(1) // gotta drop the '$'
    val resObj: Class[_] = Class.forName(objectName, true, classLoader())

    val res =
      resObj
        .getDeclaredMethods.find(_.getName == sym.name.toString + "Show").get
        .invoke(null).toString

    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else if (res.startsWith(str.REPL_SESSION_LINE))
      Some(res.drop(str.REPL_SESSION_LINE.length).dropWhile(c => c.isDigit || c == '$'))
    else
      Some(res)
  }

  /** Render method definition result */
  def renderMethod(d: Denotation)(implicit ctx: Context): String =
    d.symbol.showUser

  /** Render value definition result */
  def renderVal(d: Denotation)(implicit ctx: Context): Either[String,Option[String]] = {
    val dcl = d.symbol.showUser

    Try {
      val resultValue =
        if (d.symbol.is(Flags.Lazy)) Some("<lazy>")
        else valueOf(d.symbol)

      resultValue.map(value => s"$dcl = $value")
    } match {
      case Success(s) => Right(s)
      case Failure(ex: InvocationTargetException) => Left(renderError(ex))
      case Failure(e) => Left("<evaluation error>")
    }

  }

  def renderVal2(d: Denotation)(implicit ctx: Context): Try[Option[String]] = {
    val dcl = d.symbol.showUser

    Try {
      val resultValue =
        if (d.symbol.is(Flags.Lazy)) Some("<lazy>")
        else valueOf(d.symbol)

      resultValue.map(value => s"$dcl = $value")
    }

  }

  /** Render the stack trace of the underlying exception */
  def renderError(ex: InvocationTargetException): String = {
    val cause = ex.getCause match {
      case ex: ExceptionInInitializerError => ex.getCause
      case ex => ex
    }
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    cause.printStackTrace(pw)
    sw.toString
  }
}
