package dotty.tools
package repl

import scala.language.unsafeNulls

import dotc.*, core.*
import Contexts.*, Decorators.*, Denotations.*, Flags.*, NameOps.*, StdNames.*, Symbols.*
import printing.ReplPrinter
import printing.SyntaxHighlighting
import reporting.Diagnostic
import StackTraceOps.*

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.tools.asm.*
import scala.tools.asm.Opcodes.*
import scala.tools.asm.tree.*
import scala.util.control.NonFatal
import java.util.function.Predicate

import dotty.vendored.fansi

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

  // Temporary fix until `pprint` special-cases these.
  // (We cannot use, e.g., `isInstanceOf[LazyList]` because we're not in the same classloader)
  private val forcedToStringClasses = Set(
    "scala.collection.mutable.StringBuilder", // not technically needed but quite ugly to print as an iterable of characters
    "scala.xml.Elem" // is a Seq that contains itself, https://github.com/scala/scala3/issues/25691
  )

  private object ProductToStringProbe {

    def predicate(using Context): Predicate[Any] = (value: Any) => {
      value != null && {
        val clazz = value.getClass
        val toStringDeclaringClass = userToStringDeclaringClass(clazz)
        val testWithContext = fromSymbol.borrow(ctx) {
          fromSymbol.cache.get(clazz).orElse(
            toStringDeclaringClass.filter(_ != clazz).flatMap(fromSymbol.cache.get)
          )
        }

        testWithContext.getOrElse(toStringDeclaringClass.exists(bytecodeCache.get))
      }
    }

    /** provides access to a Context for a closure that by contract should not retain
     * after return
     */
    private trait BorrowContext {
      private var borrowed: Context = uninitialized

      def useContext[T](op: Context ?=> T): T = synchronized {
        require(borrowed != null, "BorrowContext.access called without a borrowed context")
        op(using borrowed)
      }

      def borrow[T](ctx: Context)(op: => T): T = synchronized {
        if borrowed == null then
          borrowed = ctx
          try op
          finally borrowed = null
        else op
      }
    }

    private object fromSymbol extends BorrowContext {
      val cache = classValue { clazz =>
        useContext(classHasUserDefinedToString(clazz))
      }
    }

    private val bytecodeCache = classValue(hasRuntimeUserDefinedToString(_))

    private def classValue[T](op: Class[?] => T): ClassValue[T] =
      new ClassValue[T] {
        def computeValue(clazz: Class[?]): T = op(clazz)
      }

    private def classHasUserDefinedToString(clazz: Class[?])(using Context): Option[Boolean] =
      val classSym = runtimeClassSymbol(clazz)
      if !classSym.exists || !classSym.isClass then None
      else
        val toStringSym = defn.Any_toString.matchingMember(classSym.asClass.thisType)
        if !toStringSym.exists then None
        else
          Some(toStringSym != defn.Any_toString
            && !toStringSym.is(Deferred)
            && !toStringSym.is(Synthetic))

    private def userToStringDeclaringClass(clazz: Class[?]): Option[Class[?]] =
      try
        val declaringClass = clazz.getMethod("toString").getDeclaringClass
        if declaringClass == classOf[Object] then None else Some(declaringClass)
      catch case NonFatal(_) => None

    private def hasRuntimeUserDefinedToString(declaringClass: Class[?]): Boolean =
      try
        // Some classpath-local products do not resolve back to their TASTy symbol.
        // Reject synthesized case-class toString by cracking its bytecode.
        !isScalaRunTimeProductToString(declaringClass)
      catch case NonFatal(_) => false

    private def isScalaRunTimeProductToString(clazz: Class[?]): Boolean =
      def classBytes: Array[Byte] | Null =
        val resourceName = clazz.getName.replace('.', '/') + ".class"
        val loader = clazz.getClassLoader
        val stream =
          if loader == null then ClassLoader.getSystemResourceAsStream(resourceName)
          else loader.getResourceAsStream(resourceName)
        if stream == null then null
        else
          try stream.readAllBytes()
          finally stream.close()

      def isScalaRunTimeModule(insn: AbstractInsnNode): Boolean = insn match
        case insn: FieldInsnNode =>
          insn.getOpcode == GETSTATIC
            && insn.owner == "scala/runtime/ScalaRunTime$"
            && insn.name == "MODULE$"
        case _ => false

      def isLoadThis(insn: AbstractInsnNode): Boolean = insn match
        case insn: VarInsnNode => insn.getOpcode == ALOAD && insn.`var` == 0
        case _ => false

      def isScalaRunTimeToString(insn: AbstractInsnNode): Boolean = insn match
        case insn: MethodInsnNode =>
          (insn.getOpcode == INVOKEVIRTUAL || insn.getOpcode == INVOKESTATIC)
            && (insn.owner == "scala/runtime/ScalaRunTime$" || insn.owner == "scala/runtime/ScalaRunTime")
            && insn.name == "_toString"
            && insn.desc == "(Lscala/Product;)Ljava/lang/String;"
        case _ => false

      def isReturn(insn: AbstractInsnNode): Boolean =
        insn.getOpcode == ARETURN

      def isSynthesizedToString(method: MethodNode): Boolean =
        method.instructions.iterator.asScala.filter(_.getOpcode >= 0).toList match
          case List(module, loadThis, call, ret) =>
            isScalaRunTimeModule(module) && isLoadThis(loadThis) && isScalaRunTimeToString(call) && isReturn(ret)
          case List(loadThis, call, ret) =>
            isLoadThis(loadThis) && isScalaRunTimeToString(call) && isReturn(ret)
          case _ => false

      val bytes = classBytes
      if bytes == null then false
      else
        val classNode = ClassNode()
        ClassReader(bytes).accept(classNode, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
        classNode.methods.asScala.exists: method =>
          method.name == "toString"
            && method.desc == "()Ljava/lang/String;"
            && isSynthesizedToString(method)

    private def runtimeClassSymbol(clazz: Class[?])(using Context): Symbol = {
      def getClassFromName(className: String | Null): Symbol =
        if className == null then NoSymbol
        else
          val name = className.nn.toTypeName
          val direct = getClassIfDefined(name)
          if direct.exists then direct
          else getClassIfDefined(name.unmangleClassName)

      def getMemberClass: Symbol =
        val enclosingClass = clazz.getEnclosingClass
        if enclosingClass == null then NoSymbol
        else
          val owner = runtimeClassSymbol(enclosingClass)
          val name = clazz.getSimpleName.toTypeName.unmangleClassName
          def lookup(owner: Symbol): Symbol =
            if owner.exists then owner.info.member(name).symbol else NoSymbol

          val direct = lookup(owner)
          if direct.exists then direct
          else if owner.exists then lookup(owner.linkedClass)
          else NoSymbol

      if clazz.isPrimitive || clazz.isArray then NoSymbol
      else
        val fromCanonicalName = getClassFromName(clazz.getCanonicalName)
        if fromCanonicalName.exists then fromCanonicalName
        else
          val fromMemberClass = getMemberClass
          if fromMemberClass.exists then fromMemberClass
          else getClassFromName(clazz.getName)
    }
  }

  private def pprintRender(value: Any, width: Int, height: Int, initialOffset: Int)(using Context): String = {
    val useProductToString = ProductToStringProbe.predicate

    def fallback() =
      dotty.vendored.pprint.PPrinter.Color
        .applyWithProductToString(
          value,
          width = width,
          height = height,
          indent = 2,
          initialOffset = initialOffset,
          escapeUnicode = false,
          showFieldNames = true,
          useProductToString = useProductToString
        )
        .plainText
    try
      if value != null && forcedToStringClasses(value.getClass.getName) then return value.toString
      // normally, if we used vanilla JDK and layered classloaders, we wouldn't need reflection.
      // however PPrint works by runtime type testing to deconstruct values. This is
      // sensitive to which classloader instantiates the object under test, i.e.
      // `value` is constructed inside the repl classloader. Testing for
      // `value.isInstanceOf[scala.Product]` in this classloader fails (JDK AppClassLoader),
      // because repl classloader has two layers where it can redefine `scala.Product`:
      // - `new URLClassLoader` constructed with contents of the `-classpath` setting
      // - `AbstractFileClassLoader` also might instrument the library code to support interrupt.
      // Due to the possible interruption instrumentation, it is unlikely that we can get
      // rid of reflection here.
      val cl = classLoader()
      val pprintCls = Class.forName("dotty.vendored.pprint.PPrinter$Color$", false, cl)
      val fansiStrCls = Class.forName("dotty.vendored.fansi.Str", false, cl)
      val Color = pprintCls.getField("MODULE$").get(null)
      val Color_apply = pprintCls.getMethod("applyWithProductToString",
        classOf[Any],     // value
        classOf[Int],     // width
        classOf[Int],     // height
        classOf[Int],     // indentation
        classOf[Int],     // initialOffset
        classOf[Boolean], // escape Unicode
        classOf[Boolean], // show field names
        classOf[Predicate[?]], // use product toString
      )
      val FansiStr_render = fansiStrCls.getMethod("render")
      val fansiStr = Color_apply.invoke(
        Color, value, width, height, 2, initialOffset, false, true, useProductToString
      )
      FansiStr_render.invoke(fansiStr).asInstanceOf[String]
    catch
      // Only use the fallback when not debugging, so we catch problems in unit tests
      case ex: ClassNotFoundException if !ctx.debug => fallback()
      case ex: NoSuchMethodException if !ctx.debug => fallback()
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
    val res = pprintRender(
      value,
      width = ctx.settings.pageWidth.value,
      height = ctx.settings.XreplPrintHeight.value,
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
