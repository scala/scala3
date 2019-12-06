package dotty.tools
package repl

import java.io.{ StringWriter, PrintWriter }
import java.lang.{ ClassLoader, ExceptionInInitializerError }
import java.lang.reflect.InvocationTargetException

import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Symbols.Symbol
import dotc.core.StdNames.str

/** This rendering object uses `ClassLoader`s to accomplish crossing the 4th
 *  wall (i.e. fetching back values from the compiled class files put into a
 *  specific class loader capable of loading from memory) and rendering them.
 *
 *  @pre this object should be paired with a compiler session, i.e. when
 *       `ReplDriver#resetToInitial` is called, the accompanying instance of
 *       `Rendering` is no longer valid.
 */
private[repl] class Rendering(parentClassLoader: Option[ClassLoader] = None) {

  private val MaxStringElements: Int = 1000  // no need to mkString billions of elements

  private var myClassLoader: ClassLoader = _

  private var myReplStringOf: Object => String = _


  /** Class loader used to load compiled code */
  private[repl] def classLoader()(implicit ctx: Context) =
    if (myClassLoader != null) myClassLoader
    else {
      val parent = parentClassLoader.getOrElse {
        val compilerClasspath = ctx.platform.classPath(ctx).asURLs
        new java.net.URLClassLoader(compilerClasspath.toArray, null)
      }

      myClassLoader = new AbstractFileClassLoader(ctx.settings.outputDir.value, parent)
      myReplStringOf = {
        // We need to use the ScalaRunTime class coming from the scala-library
        // on the user classpath, and not the one available in the current
        // classloader, so we use reflection instead of simply calling
        // `ScalaRunTime.replStringOf`.
        val scalaRuntime = Class.forName("scala.runtime.ScalaRunTime", true, myClassLoader)
        val meth = scalaRuntime.getMethod("replStringOf", classOf[Object], classOf[Int])

        (value: Object) => meth.invoke(null, value, Integer.valueOf(MaxStringElements)).asInstanceOf[String]
      }
      myClassLoader
    }

  /** Return a String representation of a value we got from `classLoader()`. */
  private[repl] def replStringOf(value: Object)(implicit ctx: Context): String = {
    assert(myReplStringOf != null,
      "replStringOf should only be called on values creating using `classLoader()`, but `classLoader()` has not been called so far")
    myReplStringOf(value)
  }

  /** Load the value of the symbol using reflection.
   *
   *  Calling this method evaluates the expression using reflection
   */
  private def valueOf(sym: Symbol)(implicit ctx: Context): Option[String] = {
    val defn = ctx.definitions
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resObj: Class[?] = Class.forName(objectName, true, classLoader())
    val value =
      resObj
        .getDeclaredMethods.find(_.getName == sym.name.encode.toString)
        .map(_.invoke(null))
    val string = value.map(replStringOf(_).trim)
    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else
      string.map { s =>
        if (s.startsWith(str.REPL_SESSION_LINE))
          s.drop(str.REPL_SESSION_LINE.length).dropWhile(c => c.isDigit || c == '$')
        else
          s
      }
  }

  /** Render method definition result */
  def renderMethod(d: Denotation)(implicit ctx: Context): String =
    d.symbol.showUser

  /** Render value definition result */
  def renderVal(d: Denotation)(implicit ctx: Context): Option[String] = {
    val dcl = d.symbol.showUser

    try {
      if (d.symbol.is(Flags.Lazy)) Some(dcl)
      else valueOf(d.symbol).map(value => s"$dcl = $value")
    }
    catch { case ex: InvocationTargetException => Some(renderError(ex)) }
  }

  /** Render the stack trace of the underlying exception */
  private def renderError(ex: InvocationTargetException): String = {
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
