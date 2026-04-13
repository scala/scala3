package dotty.tools
package dotc
package cc

import core.*
import Symbols.*
import Annotations.*
import util.Spans.NoSpan
import util.SrcPos
import Contexts.Context
import Constants.Constant
import Decorators.*
import ast.tpd.*
import SymDenotations.*
import Flags.*
import Types.*
import config.Feature
import typer.ProtoTypes.SelectionProto

/** Check whether references from safe mode should be allowed */
object SafeRefs {

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
  def init()(using Context): Unit =
    assumeSafe("scala.Predef", except = List("print", "println", "printf"))
    assumeSafe("java.lang.Object")
    assumeSafe("java.lang.Boolean")
    assumeSafe("java.lang.Byte")
    assumeSafe("java.lang.Char")
    assumeSafe("java.lang.Short")
    assumeSafe("java.lang.Integer")
    assumeSafe("java.lang.Long")
    assumeSafe("java.lang.Float")
    assumeSafe("java.lang.Double")
    assumeSafe("java.lang.Enum")
    assumeSafe("java.lang.Math")
    assumeSafe("java.lang.StrictMath")
    assumeSafe("java.lang.Number")
    assumeSafe("java.lang.String")
    assumeSafe("java.lang.Throwable")
    assumeSafe("java.lang.Void")
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
    rejectSafe("scala.Console")
    rejectSafe("scala.unchecked")
    rejectSafe("scala.annoation.unchecked.uncheckedOverride")
    rejectSafe("scala.annotation.unchecked.uncheckedStable")
    rejectSafe("scala.annotation.unchecked.uncheckedVariance")
    rejectSafe("scala.annotation.unchecked.uncheckedCaptures")

  def isAssumedSafe(sym: Symbol)(using Context): Boolean =
    sym.hasAnnotation(defn.AssumeSafeAnnot)
    || sym.topLevelClass.maybeOwner == defn.ScalaPackageClass
    || sym.isContainedIn(defn.ScalaCollectionImmutablePackageClas)
    || sym.isContainedIn(defn.ScalaRuntimePackageClass)

  private def fail(sym: Symbol, reason: String, pos: SrcPos)(using Context) =
    report.error(em"Cannot refer to ${sym.sanitizedDescription} from safe code since $reason", pos)
    false

  private def checkNotRejected(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    sym.getAnnotation(defn.RejectSafeAnnot) match
      case Some(annot) =>
        val message = annot.argumentConstantString(0).getOrElse("")
          fail(sym, if message.nonEmpty then message else i"it is tagged @rejectSafe", pos)
      case _ =>
        sym.owner.is(Package) || checkNotRejected(sym.owner, pos)

  def checkSafe(tree: Tree, pt: Type)(using Context): Unit = {

    def isSafe(sym: Symbol): Boolean =
      !sym.is(Package)
      && (
        isAssumedSafe(sym)
        || isSafe(if sym.is(ModuleVal) then sym.moduleClass else sym.owner))

    val (sym, checkLater) = tree match
      case tree: New => (tree.tpt.symbol, false)
      case tree: RefTree => (tree.symbol, !tree.symbol.is(Method) && pt.isInstanceOf[SelectionProto])

    if Feature.safeEnabled
        && checkNotRejected(sym, tree.srcPos)
        && !checkLater
        && sym.isStatic // if it's not static it is local, a parameter, or comes from another symbol,
                        // which has been checked
        && !sym.is(Package)
        && !isSafe(sym)
    then
      fail(sym, "it is neither compiled in safe mode nor tagged with @assumedSafe", tree.srcPos)
  }

  private def checkSafeAnnot(ann: Annotation, pos: SrcPos)(using Context): Unit =
    var errpos = ann.tree.srcPos
    if !pos.sourcePos.exists then errpos = pos
    checkNotRejected(ann.symbol, errpos)

  def checkSafeAnnots(sym: Symbol)(using Context): Unit =
    if Feature.safeEnabled then
      for ann <- sym.annotations do
        checkSafeAnnot(ann, sym.srcPos)

  def checkSafeAnnotsInType(tree: Tree)(using Context): Unit =
    def checkAnnotatedType(tp: Type) = tp match
      case AnnotatedType(tp, ann) => checkSafeAnnot(ann, tree.srcPos)
      case _ =>
    if Feature.safeEnabled then
      tree.tpe.foreachPart(checkAnnotatedType(_))
}
