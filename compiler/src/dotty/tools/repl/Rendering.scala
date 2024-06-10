package dotty.tools
package repl

import scala.language.unsafeNulls

import dotc.*, core.*
import Contexts.*, Denotations.*, Flags.*, NameOps.*, StdNames.*, Symbols.*
import printing.ReplPrinter
import reporting.Diagnostic
import util.StackTraceOps.*

import scala.compiletime.uninitialized
import scala.util.control.NonFatal

/** This rendering object uses `ClassLoader`s to accomplish crossing the 4th
 *  wall (i.e. fetching back values from the compiled class files put into a
 *  specific class loader capable of loading from memory) and rendering them.
 *
 *  @pre this object should be paired with a compiler session, i.e. when
 *       `ReplDriver#resetToInitial` is called, the accompanying instance of
 *       `Rendering` is no longer valid.
 */
private[repl] class Rendering(parentClassLoader: Option[ClassLoader] = None):

  import Rendering.*

  var myClassLoader: AbstractFileClassLoader = uninitialized

  /** (value, maxElements, maxCharacters) => String */
  var myReplStringOf: (Object, Int, Int) => String = uninitialized

  /** Class loader used to load compiled code */
  private[repl] def classLoader()(using Context) =
    if (myClassLoader != null && myClassLoader.root == ctx.settings.outputDir.value) myClassLoader
    else {
      val parent = Option(myClassLoader).orElse(parentClassLoader).getOrElse {
        val compilerClasspath = ctx.platform.classPath(using ctx).asURLs
        // We can't use the system classloader as a parent because it would
        // pollute the user classpath with everything passed to the JVM
        // `-classpath`. We can't use `null` as a parent either because on Java
        // 9+ that's the bootstrap classloader which doesn't contain modules
        // like `java.sql`, so we use the parent of the system classloader,
        // which should correspond to the platform classloader on Java 9+.
        val baseClassLoader = ClassLoader.getSystemClassLoader.getParent
        new java.net.URLClassLoader(compilerClasspath.toArray, baseClassLoader)
      }

      myClassLoader = new AbstractFileClassLoader(ctx.settings.outputDir.value, parent)
      myReplStringOf = {
        // We need to use the ScalaRunTime class coming from the scala-library
        // on the user classpath, and not the one available in the current
        // classloader, so we use reflection instead of simply calling
        // `ScalaRunTime.stringOf`. Also probe for new stringOf that does string quoting, etc.
        val scalaRuntime = Class.forName("scala.runtime.ScalaRunTime", true, myClassLoader)
        val renderer = "stringOf"
        val stringOfInvoker: (Object, Int) => String =
          def richStringOf: (Object, Int) => String =
            val method = scalaRuntime.getMethod(renderer, classOf[Object], classOf[Int], classOf[Boolean])
            val richly = java.lang.Boolean.TRUE // add a repl option for enriched output
            (value, maxElements) => method.invoke(null, value, maxElements, richly).asInstanceOf[String]
          def poorStringOf: (Object, Int) => String =
            try
              val method = scalaRuntime.getMethod(renderer, classOf[Object], classOf[Int])
              (value, maxElements) => method.invoke(null, value, maxElements).asInstanceOf[String]
            catch case _: NoSuchMethodException => (value, maxElements) => String.valueOf(value).take(maxElements)
          try richStringOf
          catch case _: NoSuchMethodException => poorStringOf
        def stringOfMaybeTruncated(value: Object, maxElements: Int): String = stringOfInvoker(value, maxElements)

        // require value != null
        // `ScalaRuntime.stringOf` returns null iff value.toString == null, let caller handle that.
        // `ScalaRuntime.stringOf` may truncate the output, in which case we want to indicate that fact to the user
        // In order to figure out if it did get truncated, we invoke it twice - once with the `maxElements` that we
        // want to print, and once without a limit. If the first is shorter, truncation did occur.
        // Note that `stringOf` has new API in flight to handle truncation, see stringOfMaybeTruncated.
        (value: Object, maxElements: Int, maxCharacters: Int) =>
          stringOfMaybeTruncated(value, Int.MaxValue) match
            case null => null
            case notTruncated =>
              val maybeTruncated =
                val maybeTruncatedByElementCount = stringOfMaybeTruncated(value, maxElements)
                truncate(maybeTruncatedByElementCount, maxCharacters)
              // our string representation may have been truncated by element and/or character count
              // if so, append an info string - but only once
              if notTruncated.length == maybeTruncated.length then maybeTruncated
              else s"$maybeTruncated ... large output truncated, print value to show all"
      }
      myClassLoader
    }

  private[repl] def truncate(str: String, maxPrintCharacters: Int)(using ctx: Context): String =
    val ncp = str.codePointCount(0, str.length) // to not cut inside code point
    if ncp <= maxPrintCharacters then str
    else str.substring(0, str.offsetByCodePoints(0, maxPrintCharacters - 1))

  /** Return a String representation of a value we got from `classLoader()`. */
  private[repl] def replStringOf(sym: Symbol, value: Object)(using Context): String =
    assert(myReplStringOf != null,
      "replStringOf should only be called on values creating using `classLoader()`, but `classLoader()` has not been called so far")
    val maxPrintElements = ctx.settings.VreplMaxPrintElements.valueIn(ctx.settingsState)
    val maxPrintCharacters = ctx.settings.VreplMaxPrintCharacters.valueIn(ctx.settingsState)
    // stringOf returns null if value.toString returns null. Show some text as a fallback.
    def fallback = s"""null // result of "${sym.name}.toString" is null"""
    if value == null then "null" else
      myReplStringOf(value, maxPrintElements, maxPrintCharacters) match
        case null => fallback
        case res  => res
    end if

  /** Load the value of the symbol using reflection.
   *
   *  Calling this method evaluates the expression using reflection
   */
  private def valueOf(sym: Symbol)(using Context): Option[String] =
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resObj: Class[?] = Class.forName(objectName, true, classLoader())
    val symValue = resObj
      .getDeclaredMethods
      .find(method => method.getName == sym.name.encode.toString && method.getParameterCount == 0)
      .flatMap(result => rewrapValueClass(sym.info.classSymbol, result.invoke(null)))
    symValue
      .filter(_ => sym.is(Flags.Method) || sym.info != defn.UnitType)
      .map(value => stripReplPrefix(replStringOf(sym, value)))

  private def stripReplPrefix(s: String): String =
    if (s.startsWith(REPL_WRAPPER_NAME_PREFIX))
      s.drop(REPL_WRAPPER_NAME_PREFIX.length).dropWhile(c => c.isDigit || c == '$')
    else
      s

  /** Rewrap value class to their Wrapper class
   *
   * @param sym Value Class symbol
   * @param value underlying value
   */
  private def rewrapValueClass(sym: Symbol, value: Object)(using Context): Option[Object] =
    if sym.isDerivedValueClass then
      val valueClass = Class.forName(sym.binaryClassName, true, classLoader())
      valueClass.getConstructors.headOption.map(_.newInstance(value))
    else
      Some(value)

  def renderTypeDef(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined " ++ d.symbol.showUser, d)

  def renderTypeAlias(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined alias " ++ d.symbol.showUser, d)

  /** Render method definition result */
  def renderMethod(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic(d.symbol.showUser, d)

  /** Render value definition result */
  def renderVal(d: Denotation)(using Context): Either[ReflectiveOperationException, Option[Diagnostic]] =
    val dcl = d.symbol.showUser
    def msg(s: String) = infoDiagnostic(s, d)
    try
      Right(
        if d.symbol.is(Flags.Lazy) then Some(msg(dcl))
        else valueOf(d.symbol).map(value => msg(s"$dcl = $value"))
      )
    catch case e: ReflectiveOperationException => Left(e)
  end renderVal

  /** Force module initialization in the absence of members. */
  def forceModule(sym: Symbol)(using Context): Seq[Diagnostic] =
    def load() =
      val objectName = sym.fullName.encode.toString
      Class.forName(objectName, true, classLoader())
      Nil
    try load()
    catch
      case e: ExceptionInInitializerError => List(renderError(e, sym.denot))
      case NonFatal(e) => List(renderError(e, sym.denot))

  /** Render the stack trace of the underlying exception. */
  def renderError(thr: Throwable, d: Denotation)(using Context): Diagnostic =
    val cause = rootCause(thr)
    // detect
    //at repl$.rs$line$2$.<clinit>(rs$line$2:1)
    //at repl$.rs$line$2.res1(rs$line$2)
    def isWrapperInitialization(ste: StackTraceElement) =
      ste.getClassName.startsWith(REPL_WRAPPER_NAME_PREFIX)  // d.symbol.owner.name.show is simple name
      && (ste.getMethodName == nme.STATIC_CONSTRUCTOR.show || ste.getMethodName == nme.CONSTRUCTOR.show)

    infoDiagnostic(cause.formatStackTracePrefix(!isWrapperInitialization(_)), d)
  end renderError

  private def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(msg, d.symbol.sourcePos)

object Rendering:
  final val REPL_WRAPPER_NAME_PREFIX = str.REPL_SESSION_LINE

  extension (s: Symbol)
    def showUser(using Context): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
    }

  def rootCause(x: Throwable): Throwable = x match
    case _: ExceptionInInitializerError |
         _: java.lang.reflect.InvocationTargetException |
         _: java.lang.reflect.UndeclaredThrowableException |
         _: java.util.concurrent.ExecutionException
        if x.getCause != null =>
      rootCause(x.getCause)
    case _ => x
