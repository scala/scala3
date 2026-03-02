package dotty.tools
package repl

import scala.language.unsafeNulls

import dotc.*, core.*
import Contexts.*, Denotations.*, Flags.*, NameOps.*, StdNames.*, Symbols.*
import printing.ReplPrinter
import printing.SyntaxHighlighting
import reporting.Diagnostic
import StackTraceOps.*

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

  private def pprintRender(value: Any, width: Int, height: Int, initialOffset: Int)(using Context): String = {
    def fallback() =
      pprint.PPrinter.Color
        .apply(value, width = width, height = height, initialOffset = initialOffset)
        .plainText
    try
      // normally, if we used vanilla JDK and layered classloaders, we wouldnt need reflection.
      // however PPrint works by runtime type testing to deconstruct values. This is
      // sensitive to which classloader instantiates the object under test, i.e.
      // `value` is constructed inside the repl classloader. Testing for
      // `value.isInstanceOf[scala.Product]` in this classloader fails (JDK AppClassLoader),
      // because repl classloader has two layers where it can redefine `scala.Product`:
      // - `new URLClassLoader` constructed with contents of the `-classpath` setting
      // - `AbstractFileClassLoader` also might instrument the library code to support interrupt.
      // Due the possible interruption instrumentation, it is unlikely that we can get
      // rid of reflection here.
      val cl = classLoader()
      val pprintCls = Class.forName("pprint.PPrinter$Color$", false, cl)
      val fansiStrCls = Class.forName("fansi.Str", false, cl)
      val Color = pprintCls.getField("MODULE$").get(null)
      val Color_apply = pprintCls.getMethod("apply",
        classOf[Any],     // value
        classOf[Int],     // width
        classOf[Int],     // height
        classOf[Int],     // indentation
        classOf[Int],     // initialOffset
        classOf[Boolean], // escape Unicode
        classOf[Boolean], // show field names
      )
      val FansiStr_render = fansiStrCls.getMethod("render")
      val fansiStr = Color_apply.invoke(
        Color, value, width, height, 2, initialOffset, false, true
      )
      FansiStr_render.invoke(fansiStr).asInstanceOf[String]
    catch
      case ex: ClassNotFoundException => fallback()
      case ex: NoSuchMethodException  => fallback()
  }


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

      myClassLoader = new AbstractFileClassLoader(
        ctx.settings.outputDir.value,
        parent,
        AbstractFileClassLoader.InterruptInstrumentation.fromString(ctx.settings.XreplInterruptInstrumentation.value)
      )
      myClassLoader
    }

  private[repl] def truncate(str: String, maxPrintCharacters: Int)(using ctx: Context): String =
    val ncp = str.codePointCount(0, str.length) // to not cut inside code point
    if ncp <= maxPrintCharacters then str
    else str.substring(0, str.offsetByCodePoints(0, maxPrintCharacters - 1))

  /** Return a colored fansi.Str representation of a value we got from `classLoader()`. */
  private[repl] def replStringOf(value: Object, prefixLength: Int)(using Context): fansi.Str = {
    // pretty-print things with 100 cols 50 rows by default,
    val res = pprintRender(
      value,
      width = 100,
      height = 50,
      initialOffset = prefixLength
    )
    if (ctx.settings.color.value == "never") fansi.Str(res).plainText else res
  }

  /** Load the value of the symbol using reflection.
   *
   *  Calling this method evaluates the expression using reflection
   */
  private def valueOf(sym: Symbol, prefixLength: Int)(using Context): Option[fansi.Str] =
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resObj: Class[?] = Class.forName(objectName, true, classLoader())
    val symValue = resObj
      .getDeclaredMethods
      .find(method => method.getName == sym.name.encode.toString && method.getParameterCount == 0)
      .flatMap(result => rewrapValueClass(sym.info.classSymbol, result.invoke(null)))
    symValue
      .filter(_ => sym.is(Flags.Method) || sym.info != defn.UnitType)
      .map(value => stripReplPrefixFansi(replStringOf(value, prefixLength)))

  private def stripReplPrefixFansi(s: fansi.Str): fansi.Str =
    val plain = s.plainText
    if (plain.startsWith(REPL_WRAPPER_NAME_PREFIX))
      val prefixLen = REPL_WRAPPER_NAME_PREFIX.length
      val dropLen = prefixLen + plain.drop(prefixLen).takeWhile(c => c.isDigit || c == '$').length
      s.substring(dropLen)
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
    val dcl = fansi.Str(SyntaxHighlighting.highlight(d.symbol.showUser))
    def msg(s: fansi.Str) = infoDiagnostic(s, d)
    try
      Right(
        if d.symbol.is(Flags.Lazy) then Some(msg(dcl))
        else {
          val prefix = dcl ++ " = "
          // Prefix can have multiple lines, only consider the last one
          // when determining the initial column offset for pretty-printing
          val prefixLength = prefix.plainText.linesIterator.toSeq.lastOption.getOrElse("").length
          valueOf(d.symbol, prefixLength).map(value => msg(prefix ++ value))
        }
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

    val formatted0 = cause.formatStackTracePrefix(!isWrapperInitialization(_))
    val formatted: fansi.Str = if (ctx.settings.color.value == "never") formatted0.plainText else formatted0
    infoDiagnostic(formatted, d)
  end renderError

  private def infoDiagnostic(msg: fansi.Str, d: Denotation)(using Context): Diagnostic = {
    new Diagnostic.Info(msg.render, d.symbol.sourcePos)
  }

  private def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(SyntaxHighlighting.highlight(msg), d.symbol.sourcePos)

object Rendering:
  final val REPL_WRAPPER_NAME_PREFIX = str.REPL_SESSION_LINE

  extension (s: Symbol)
    def showUser(using Context): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value)
    }

  def rootCause(x: Throwable): Throwable = x match
    case _: ExceptionInInitializerError |
         _: java.lang.reflect.InvocationTargetException |
         _: java.lang.reflect.UndeclaredThrowableException |
         _: java.util.concurrent.ExecutionException
        if x.getCause != null =>
      rootCause(x.getCause)
    case _ => x
