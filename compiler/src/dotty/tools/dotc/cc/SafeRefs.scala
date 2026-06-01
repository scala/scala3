package dotty.tools
package dotc
package cc

import core.*
import Symbols.*
import Annotations.*
import util.Spans.NoSpan
import util.{Property, SrcPos}
import Contexts.{Context, ctx}
import Constants.Constant
import Decorators.*
import ast.tpd.*
import SymDenotations.*
import Flags.*
import Types.*
import Names.Name
import NameOps.isReplWrapperName
import config.Printers.capt

/** Check whether references from safe mode should be allowed */
object SafeRefs {

  val assumedSafePackages = List(
    "scala", "scala.runtime", "scala.collection.immutable", "scala.compiletime.ops",
    "scala.math", "scala.util", "scala.caps", "java.math", "java.time",
    "java.util.function", "java.util.regex", "java.util.stream"
  )

  private def rejectSafe(sym: Symbol)(using Context): Unit =
    if !sym.infoOrCompleter.isInstanceOf[StubInfo] then
      sym.addAnnotation(Annotation(defn.RejectSafeAnnot, List(Literal(Constant(""))), NoSpan))
      if sym.is(ModuleVal) then rejectSafe(sym.moduleClass)

  private def assumeSafe(sym: Symbol, except: List[String])(using Context): Unit =
    if !sym.infoOrCompleter.isInstanceOf[StubInfo] then
      val cls = if sym.is(ModuleVal) then sym.moduleClass else sym
      cls.addAnnotation(Annotation(defn.AssumeSafeAnnot, NoSpan))
      for exc <- except
          name <- List(exc.toTermName, exc.toTypeName)
          mbr <- cls.info.member(name).alternatives
      do
        rejectSafe(mbr.symbol)

  private def assumeSafe(name: String, except: List[String] = Nil)(using Context): Unit =
    assumeSafe(requiredClass(name), except)
    assumeSafe(requiredModule(name), except)

  private def rejectSafe(name: String)(using Context): Unit =
    rejectSafe(requiredClass(name))
    rejectSafe(requiredModule(name))

  /** Declare some classes and methods @assumeSafe or @rejectSafe. We can't (currently) put in
   *  annotations directly since some of these come from Java and others first need a bootstrapped
   *  compiler that updates Feature.ccException to allow experimental @assumeSafe and @rejectSafe.
   *  Once we have an updated ccException in the bootstrap compiler, we could add annotations
   *  to library classes manually, as long as these library classes are capture checked.
   */
  def init()(using Context): Unit = {
    assumeSafe("scala.Predef", except = List("print", "println", "printf"))
    assumeSafe("scala.runtime.coverage.Invoker")
    assumeSafe("scala.reflect.ClassTag")
    assumeSafe("scala.util.Properties", except = List("setProp", "clearProp", "main"))
    assumeSafe("java.lang.Object")
    assumeSafe("java.lang.Boolean")
    assumeSafe("java.lang.Byte")
    assumeSafe("java.lang.Char")
    assumeSafe("java.lang.Character")
    assumeSafe("java.lang.Short")
    assumeSafe("java.lang.Integer")
    assumeSafe("java.lang.Long")
    assumeSafe("java.lang.Float")
    assumeSafe("java.lang.Double")
    assumeSafe("java.lang.Void")
    assumeSafe("java.lang.Enum")
    assumeSafe("java.lang.Math")
    assumeSafe("java.lang.StrictMath")
    assumeSafe("java.lang.Number")
    assumeSafe("java.lang.String")
    assumeSafe("java.lang.Throwable")
    assumeSafe("java.lang.Exception")
    assumeSafe("java.lang.RuntimeException")
    assumeSafe("java.lang.ArithmeticException")
    assumeSafe("java.lang.ArrayIndexOutOfBoundsException")
    assumeSafe("java.lang.ArrayStoreException")
    assumeSafe("java.lang.ClassCastException")
    assumeSafe("java.lang.ClassNotFoundException")
    assumeSafe("java.lang.CloneNotSupportedException")
    assumeSafe("java.lang.EnumConstantNotPresentException")
    assumeSafe("java.lang.IllegalAccessException")
    assumeSafe("java.lang.IllegalArgumentException")
    assumeSafe("java.lang.IllegalMonitorStateException")
    assumeSafe("java.lang.IllegalStateException")
    assumeSafe("java.lang.IllegalThreadStateException")
    assumeSafe("java.lang.IndexOutOfBoundsException")
    assumeSafe("java.lang.InstantiationException")
    assumeSafe("java.lang.InterruptedException")
    assumeSafe("java.lang.NegativeArraySizeException")
    assumeSafe("java.lang.NoSuchFieldException")
    assumeSafe("java.lang.NoSuchMethodException")
    assumeSafe("java.lang.NullPointerException")
    assumeSafe("java.lang.NumberFormatException")
    assumeSafe("java.lang.ReflectiveOperationException")
    assumeSafe("java.lang.SecurityException")
    assumeSafe("java.lang.StringIndexOutOfBoundsException")
    assumeSafe("java.lang.TypeNotPresentException")
    assumeSafe("java.lang.UnsupportedOperationException")
    // `java.lang.Error` and its subclasses are intentionally not marked safe:
    // they indicate serious VM-level problems and should not be caught or
    // thrown from safe code.
    assumeSafe("java.lang.StackTraceElement")
    assumeSafe("java.lang.Record")
    assumeSafe("java.lang.CharSequence")
    assumeSafe("java.lang.Comparable")
    assumeSafe("java.lang.Class", except = List(
      "accessFlags", "asSubclass", "cast", "describeConstable",
      "descriptorString", "desiredAssertionStatus", "forName", "forPrimitiveName", "getAnnotatedInterfaces",
      "getAnnotatedSuperclass", "getAnnotation", "getAnnotations", "getAnnotationsByType", "getCanonicalName",
      "getClasses", "getClassLoader", "getComponentType", "getConstructor", "getConstructors", "getDeclaredAnnotation",
      "getDeclaredAnnotations", "getDeclaredAnnotationsByType", "getDeclaredClasses", "getDeclaredConstructor",
      "getDeclaredConstructors", "getDeclaredField", "getDeclaredFields", "getDeclaredMethod", "getDeclaredMethods",
      "getDeclaringClass", "getEnclosingClass", "getEnclosingConstructor", "getEnclosingMethod", "getEnumConstants",
      "getField", "getFields", "getMethod", "getMethods", "getModifiers", "getModule", "getNestHost", "getNestMembers",
      "getPackage", "getPackageName", "getPermittedSubclasses", "getProtectionDomain", "getRecordComponents",
      "getResource", "getResourceAsStream", "getSigners", "getTypeParameters", "getTypeName",
      "newInstance", "cast", "toGenericString"))
    assumeSafe("java.util.Locale", except = List("setDefault"))
    assumeSafe("java.util.TimeZone", except = List("setDefault", "setID", "setRawOffset"))
    assumeSafe("java.util.UUID", except = List("randomUIID"))
    assumeSafe("java.util.Objects")
    assumeSafe("java.util.Optional")
    assumeSafe("java.util.OptionalInt")
    assumeSafe("java.util.OptionalLong")
    assumeSafe("java.util.OptionalDouble")
    assumeSafe("java.util.NoSuchElementException")

    rejectSafe("scala.Console")
    rejectSafe("scala.unchecked")
    rejectSafe("scala.annotation.unchecked.uncheckedOverride")
    rejectSafe("scala.annotation.unchecked.uncheckedStable")
    rejectSafe("scala.annotation.unchecked.uncheckedVariance")
    rejectSafe("scala.annotation.unchecked.uncheckedCaptures")
    rejectSafe("scala.util.DynamicVariable")
    rejectSafe("scala.util.Using") // todo capture check Using

    // Reject mutable classes in scala.runtime
    rejectSafe("scala.runtime.BooleanRef")
    rejectSafe("scala.runtime.ByteRef")
    rejectSafe("scala.runtime.CharRef")
    rejectSafe("scala.runtime.DoubleRef")
    rejectSafe("scala.runtime.FloatRef")
    rejectSafe("scala.runtime.IntRef")
    rejectSafe("scala.runtime.LongRef")
    rejectSafe("scala.runtime.ShortRef")
    rejectSafe("scala.runtime.ObjectRef")
    rejectSafe("scala.runtime.VolatileBooleanRef")
    rejectSafe("scala.runtime.VolatileByteRef")
    rejectSafe("scala.runtime.VolatileCharRef")
    rejectSafe("scala.runtime.VolatileDoubleRef")
    rejectSafe("scala.runtime.VolatileFloatRef")
    rejectSafe("scala.runtime.VolatileIntRef")
    rejectSafe("scala.runtime.VolatileLongRef")
    rejectSafe("scala.runtime.VolatileShortRef")
    rejectSafe("scala.runtime.VolatileObjectRef")
    rejectSafe("scala.runtime.LazyRef")
    rejectSafe("scala.runtime.LazyBoolean")
    rejectSafe("scala.runtime.LazyByte")
    rejectSafe("scala.runtime.LazyChar")
    rejectSafe("scala.runtime.LazyShort")
    rejectSafe("scala.runtime.LazyInt")
    rejectSafe("scala.runtime.LazyLong")
    rejectSafe("scala.runtime.LazyFloat")
    rejectSafe("scala.runtime.LazyDouble")
    rejectSafe("scala.runtime.LazyUnit")
  }

  /** Allow name in safe mode even though it contains `$` characters */
  def allowDollarIn(name: Name)(using Context): Boolean =
    name.isReplWrapperName && ctx.mode.is(Mode.Interactive)

  private def fail(sym: Symbol, reason: String, pos: SrcPos)(using Context) =
    report.error(em"Cannot refer to ${sym.sanitizedDescription}${sym.showExtendedLocation} from safe code since $reason", pos)
    false

  private def checkNotRejected(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    !sym.exists || sym.is(Package) || sym.getAnnotation(defn.RejectSafeAnnot).match
      case Some(annot) =>
        val message = annot.argumentConstantString(0).getOrElse("")
        fail(sym, if message.nonEmpty then message else i"it is tagged @rejectSafe", pos)
      case _ =>
        checkNotRejected(sym.owner, pos)

  /** Check that all nodes of given tree for the following conditions.
   *   - No reference to a symbol under a @rejectSafe annotation
   *   - All references to static symbols are assumed safe: This means
   *     they have been compiled in safe mode, or have an @assumeSafe
   *     annotation or are owned by a symbol with an @assumeSafe annotation.
   *   - No reference to a user-defined annotation which is marked @rejectSafe
   */
  object checker extends TreeTraverser:
    private var checkTypes = false
    def traverse(tree: Tree)(using Context) =
      val sym = tree.symbol
      tree match
        case tree: Ident =>
          checkNotRejected(sym, tree.srcPos)
          val isStatic = tree.tpe match
            case NamedType(prefix, _) =>
              prefix.dealias match
                case prefix: ThisType => prefix.cls.isStatic
                case prefix: TermRef => prefix.symbol.isStatic
                case _ => sym.isStatic
            case _ => sym.isStatic
          // if sym is not static it is local, a parameter, or comes from another symbol,
          // which has been checked
          if isStatic && (checkTypes || sym.isTerm) then
            checkSafe(sym, tree)
        case tree: Select =>
          checkNotRejected(sym, tree.srcPos)
          if sym.isStatic && (checkTypes || sym.isTerm)
          then checkSafe(sym, tree)
          else traverseChildren(tree)
        case New(tpt) =>
          val saved = checkTypes
          checkTypes = true
          try traverse(tpt)
          finally checkTypes = saved
        case Inlined(call, _, _) =>
          traverse(call)
        case tree: MemberDef if !sym.is(Synthetic) =>
          for ann <- sym.annotations do
            checkSafeAnnot(ann, sym.srcPos)
          traverseChildren(tree)
        case tree: TypeApply if sym == defn.Any_asInstanceOf =>
          report.error(em"Cannot use asInstanceOf in safe mode", tree.srcPos)
        case Annotated(arg, annot) =>
          checkNotRejected(annot.symbol, annot.srcPos.orElse(tree.srcPos))
          traverseChildren(arg)
        case tree: Import =>
          // skip imports, we want to be able to wildcard import from an unsafe
          // object as long as all used members are @assumeSafe
        case _ =>
          traverseChildren(tree)

  def checkSafe(sym: Symbol, tree: Tree)(using Context): Unit = {

    def isSafe(sym: Symbol): Boolean =
      if !sym.exists then false
      else if sym.is(Package) then
        defn.assumedSafePackages.contains(sym)
      else
        sym.hasAnnotation(defn.AssumeSafeAnnot)
        || isSafe(if sym.is(ModuleVal) then sym.moduleClass else sym.owner)

    if sym.exists && !sym.is(Package) && !isSafe(sym) then
      fail(sym, "it is neither compiled in safe mode nor tagged with @assumedSafe", tree.srcPos)
    else
      capt.println(i"checked safe $tree, $sym")
  }

  private def checkSafeAnnot(ann: Annotation, pos: SrcPos)(using Context): Unit =
    val span = ann.tree.span
    // Skip compiler inserted annotations that have no or zero extent span.
    if !span.exists || span.isZeroExtent then return
    checkNotRejected(ann.symbol, ann.tree.srcPos)
}
