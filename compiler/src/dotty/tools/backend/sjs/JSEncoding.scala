package dotty.tools.backend.sjs

import scala.language.unsafeNulls

import scala.collection.mutable

import dotty.tools.dotc.core._
import Contexts._
import Flags._
import Types._
import Symbols._
import NameOps._
import Names._
import StdNames._

import dotty.tools.dotc.transform.sjs.JSSymUtils._

import org.scalajs.ir
import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Names.{LocalName, LabelName, FieldName, SimpleMethodName, MethodName, ClassName}
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.UTF8String

import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

/** Encoding of symbol names for JavaScript
 *
 *  Some issues that this encoding solves:
 *  * Overloading: encode the full signature in the JS name
 *  * Same scope for fields and methods of a class
 *  * Global access to classes and modules (by their full name)
 *
 *  @author Sébastien Doeraene
 */
object JSEncoding {

  /** Name of the capture param storing the JS super class.
   *
   *  This is used by the dispatchers of exposed JS methods and properties of
   *  nested JS classes when they need to perform a super call. Other super
   *  calls (in the actual bodies of the methods, not in the dispatchers) do
   *  not use this value, since they are implemented as static methods that do
   *  not have access to it. Instead, they get the JS super class value through
   *  the magic method inserted by `ExplicitLocalJS`, leveraging `lambdalift`
   *  to ensure that it is properly captured.
   *
   *  Using this identifier is only allowed if it was reserved in the current
   *  local name scope using [[reserveLocalName]]. Otherwise, this name can
   *  clash with another local identifier.
   */
  final val JSSuperClassParamName = LocalName("superClass$")

  private val ScalaRuntimeNothingClassName = ClassName("scala.runtime.Nothing$")
  private val ScalaRuntimeNullClassName = ClassName("scala.runtime.Null$")

  private val dynamicImportForwarderSimpleName = SimpleMethodName("dynamicImport$")

  // Fresh local name generator ----------------------------------------------

  class LocalNameGenerator {
    import LocalNameGenerator._

    private val usedLocalNames = mutable.Set.empty[LocalName]
    private val localSymbolNames = mutable.Map.empty[Symbol, LocalName]
    private val usedLabelNames = mutable.Set.empty[LabelName]
    private val labelSymbolNames = mutable.Map.empty[Symbol, LabelName]
    private var returnLabelName: Option[LabelName] = None

    def reserveLocalName(name: LocalName): Unit = {
      require(usedLocalNames.isEmpty,
          s"Trying to reserve the name '$name' but names have already been allocated")
      usedLocalNames += name
    }

    private def freshNameGeneric[N <: ir.Names.Name](base: N, usedNamesSet: mutable.Set[N])(
        withSuffix: (N, String) => N): N = {

      var suffix = 1
      var result = base
      while (usedNamesSet(result)) {
        suffix += 1
        result = withSuffix(base, "$" + suffix)
      }
      usedNamesSet += result
      result
    }

    def freshName(base: LocalName): LocalName =
      freshNameGeneric(base, usedLocalNames)(_.withSuffix(_))

    def freshName(base: String): LocalName =
      freshName(LocalName(base))

    def freshLocalIdent()(implicit pos: ir.Position): js.LocalIdent =
      js.LocalIdent(freshName(xLocalName))

    def freshLocalIdent(base: LocalName)(implicit pos: ir.Position): js.LocalIdent =
      js.LocalIdent(freshName(base))

    def freshLocalIdent(base: String)(implicit pos: ir.Position): js.LocalIdent =
      freshLocalIdent(LocalName(base))

    def freshLocalIdent(base: TermName)(implicit pos: ir.Position): js.LocalIdent =
      freshLocalIdent(base.mangledString)

    def localSymbolName(sym: Symbol)(using Context): LocalName = {
      localSymbolNames.getOrElseUpdate(sym, {
        /* The emitter does not like local variables that start with a '$',
         * because it needs to encode them not to clash with emitter-generated
         * names. There are two common cases, caused by scalac-generated names:
         * - the `$this` parameter of tailrec methods and "extension" methods of
         *   AnyVals, which scalac knows as `nme.SELF`, and
         * - the `$outer` parameter of inner class constructors, which scalac
         *   knows as `nme.OUTER`.
         * We choose different base names for those two cases instead, so that
         * the avoidance mechanism of the emitter doesn't happen as a common
         * case. It can still happen for user-defined variables, but in that case
         * the emitter will deal with it.
         */
        val base = sym.name match {
          case nme.SELF  => "this$" // instead of $this
          case nme.OUTER => "outer" // instead of $outer
          case name      => name.mangledString
        }
        freshName(base)
      })
    }

    def freshLabelName(base: LabelName): LabelName =
      freshNameGeneric(base, usedLabelNames)(_.withSuffix(_))

    def freshLabelName(base: String): LabelName =
      freshLabelName(LabelName(base))

    def freshLabelIdent(base: String)(implicit pos: ir.Position): js.LabelIdent =
      js.LabelIdent(freshLabelName(base))

    def labelSymbolName(sym: Symbol)(using Context): LabelName =
      labelSymbolNames.getOrElseUpdate(sym, freshLabelName(sym.javaSimpleName))

    def getEnclosingReturnLabel()(implicit pos: ir.Position): js.LabelIdent = {
      if (returnLabelName.isEmpty)
        returnLabelName = Some(freshLabelName("_return"))
      js.LabelIdent(returnLabelName.get)
    }

    /* If this `LocalNameGenerator` has a `returnLabelName` (often added in the
     * construction of the `body` argument), wrap the resulting js.Tree to use that label.
     */
    def makeLabeledIfRequiresEnclosingReturn(tpe: jstpe.Type)(body: js.Tree)(implicit pos: ir.Position): js.Tree = {
      returnLabelName match {
        case None =>
          body
        case Some(labelName) =>
          js.Labeled(js.LabelIdent(labelName), tpe, body)
      }
    }
  }

  private object LocalNameGenerator {
    private val xLocalName = LocalName("x")
  }

  // Encoding methods ----------------------------------------------------------

  def encodeLabelSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position, localNames: LocalNameGenerator): js.LabelIdent = {
    require(sym.is(Flags.Label), "encodeLabelSym called with non-label symbol: " + sym)
    js.LabelIdent(localNames.labelSymbolName(sym))
  }

  def encodeFieldSym(sym: Symbol)(implicit ctx: Context, pos: ir.Position): js.FieldIdent =
    js.FieldIdent(FieldName(encodeFieldSymAsString(sym)))

  def encodeFieldSymAsStringLiteral(sym: Symbol)(implicit ctx: Context, pos: ir.Position): js.StringLiteral =
    js.StringLiteral(encodeFieldSymAsString(sym))

  private def encodeFieldSymAsString(sym: Symbol)(using Context): String = {
    require(sym.owner.isClass && sym.isTerm && !sym.isOneOf(MethodOrModule),
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = sym.javaSimpleName
    if (name0.charAt(name0.length() - 1) != ' ') name0
    else name0.substring(0, name0.length() - 1)
  }

  def encodeMethodSym(sym: Symbol, reflProxy: Boolean = false)(
      implicit ctx: Context, pos: ir.Position): js.MethodIdent = {
    require(sym.is(Flags.Method), "encodeMethodSym called with non-method symbol: " + sym)

    val tpe = sym.info

    val paramTypeRefs0 = tpe.firstParamTypes.map(paramOrResultTypeRef(_))

    val hasExplicitThisParameter = !sym.is(JavaStatic) && sym.owner.isNonNativeJSClass
    val paramTypeRefs =
      if (!hasExplicitThisParameter) paramTypeRefs0
      else encodeClassRef(sym.owner) :: paramTypeRefs0

    val name = sym.name
    val simpleName = SimpleMethodName(name.mangledString)

    val methodName = {
      if (sym.isClassConstructor)
        MethodName.constructor(paramTypeRefs)
      else if (reflProxy)
        MethodName.reflectiveProxy(simpleName, paramTypeRefs)
      else
        MethodName(simpleName, paramTypeRefs, paramOrResultTypeRef(patchedResultType(sym)))
    }

    js.MethodIdent(methodName)
  }

  def encodeStaticMemberSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.MethodIdent = {
    require(sym.is(Flags.JavaStaticTerm),
        "encodeStaticMemberSym called with non-static symbol: " + sym)

    val name = sym.name
    val resultTypeRef = paramOrResultTypeRef(sym.info)
    val methodName = MethodName(name.mangledString, Nil, resultTypeRef)
    js.MethodIdent(methodName)
  }

  def encodeDynamicImportForwarderIdent(params: List[Symbol])(using Context, ir.Position): js.MethodIdent = {
    val paramTypeRefs = params.map(sym => paramOrResultTypeRef(sym.info))
    val resultTypeRef = jstpe.ClassRef(ir.Names.ObjectClass)
    val methodName = MethodName(dynamicImportForwarderSimpleName, paramTypeRefs, resultTypeRef)
    js.MethodIdent(methodName)
  }

  /** Computes the type ref for a type, to be used in a method signature. */
  private def paramOrResultTypeRef(tpe: Type)(using Context): jstpe.TypeRef =
    toParamOrResultTypeRef(toTypeRef(tpe))

  def encodeLocalSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position, localNames: LocalNameGenerator): js.LocalIdent = {
    require(!sym.owner.isClass && sym.isTerm && !sym.is(Flags.Method) && !sym.is(Flags.Module),
        "encodeLocalSym called with non-local symbol: " + sym)
    js.LocalIdent(localNames.localSymbolName(sym))
  }

  def encodeClassType(sym: Symbol)(using Context): jstpe.Type = {
    if (sym == defn.ObjectClass) jstpe.AnyType
    else if (sym.isJSType) jstpe.AnyType
    else {
      assert(sym != defn.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassName(sym))
    }
  }

  def encodeClassRef(sym: Symbol)(using Context): jstpe.ClassRef =
    jstpe.ClassRef(encodeClassName(sym))

  def encodeClassNameIdent(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.ClassIdent =
    js.ClassIdent(encodeClassName(sym))

  def encodeClassName(sym: Symbol)(using Context): ClassName = {
    val sym1 =
      if (sym.isAllOf(ModuleClass | JavaDefined)) sym.linkedClass
      else sym

    /* Some rewirings:
     * - scala.runtime.BoxedUnit to java.lang.Void, as the IR expects.
     *   BoxedUnit$ is a JVM artifact.
     * - scala.Nothing to scala.runtime.Nothing$.
     * - scala.Null to scala.runtime.Null$.
     */
    if (sym1 == defn.BoxedUnitClass)
      ir.Names.BoxedUnitClass
    else if (sym1 == defn.NothingClass)
      ScalaRuntimeNothingClassName
    else if (sym1 == defn.NullClass)
      ScalaRuntimeNullClassName
    else
      ClassName(sym1.javaClassName)
  }

  /** Converts a general TypeRef to a TypeRef to be used in a method signature. */
  def toParamOrResultTypeRef(typeRef: jstpe.TypeRef): jstpe.TypeRef = {
    typeRef match {
      case jstpe.ClassRef(ScalaRuntimeNullClassName)    => jstpe.NullRef
      case jstpe.ClassRef(ScalaRuntimeNothingClassName) => jstpe.NothingRef
      case _                                            => typeRef
    }
  }

  def toIRTypeAndTypeRef(tp: Type)(using Context): (jstpe.Type, jstpe.TypeRef) = {
    val typeRefInternal = toTypeRefInternal(tp)
    (toIRTypeInternal(typeRefInternal), typeRefInternal._1)
  }

  def toIRType(tp: Type)(using Context): jstpe.Type =
    toIRTypeInternal(toTypeRefInternal(tp))

  private def toIRTypeInternal(typeRefInternal: (jstpe.TypeRef, Symbol))(using Context): jstpe.Type = {
    typeRefInternal._1 match {
      case jstpe.PrimRef(irTpe) =>
        irTpe

      case typeRef: jstpe.ClassRef =>
        val sym = typeRefInternal._2
        if (sym == defn.ObjectClass || sym.isJSType)
          jstpe.AnyType
        else if (sym == defn.NothingClass)
          jstpe.NothingType
        else if (sym == defn.NullClass)
          jstpe.NullType
        else
          jstpe.ClassType(typeRef.className)

      case typeRef: jstpe.ArrayTypeRef =>
        jstpe.ArrayType(typeRef)
    }
  }

  def toTypeRef(tp: Type)(using Context): jstpe.TypeRef =
    toTypeRefInternal(tp)._1

  private def toTypeRefInternal(tp: Type)(using Context): (jstpe.TypeRef, Symbol) = {
    def primitiveOrClassToTypeRef(sym: Symbol): (jstpe.TypeRef, Symbol) = {
      assert(sym.isClass, sym)
      //assert(sym != defn.ArrayClass || isCompilingArray, sym)
      val typeRef = if (sym.isPrimitiveValueClass) {
        if (sym == defn.UnitClass) jstpe.VoidRef
        else if (sym == defn.BooleanClass) jstpe.BooleanRef
        else if (sym == defn.CharClass) jstpe.CharRef
        else if (sym == defn.ByteClass) jstpe.ByteRef
        else if (sym == defn.ShortClass) jstpe.ShortRef
        else if (sym == defn.IntClass) jstpe.IntRef
        else if (sym == defn.LongClass) jstpe.LongRef
        else if (sym == defn.FloatClass) jstpe.FloatRef
        else if (sym == defn.DoubleClass) jstpe.DoubleRef
        else throw new Exception(s"unknown primitive value class $sym")
      } else {
        encodeClassRef(sym)
      }
      (typeRef, sym)
    }

    /**
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
     */
    def nonClassTypeRefToTypeRef(sym: Symbol): (jstpe.TypeRef, Symbol) = {
      //assert(sym.isType && isCompilingArray, sym)
      (jstpe.ClassRef(ir.Names.ObjectClass), defn.ObjectClass)
    }

    tp.widenDealias match {
      // Array type such as Array[Int] (kept by erasure)
      case JavaArrayType(el) =>
        val elTypeRef = toTypeRefInternal(el)
        (jstpe.ArrayTypeRef.of(elTypeRef._1), elTypeRef._2)

      case t: TypeRef =>
        if (!t.symbol.isClass) nonClassTypeRefToTypeRef(t.symbol)  // See comment on nonClassTypeRefToBType
        else primitiveOrClassToTypeRef(t.symbol) // Common reference to a type such as scala.Int or java.lang.String

      case Types.ClassInfo(_, sym, _, _, _) =>
        /* We get here, for example, for genLoadModule, which invokes
         * toTypeKind(moduleClassSymbol.info)
         */
        primitiveOrClassToTypeRef(sym)

      /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
       * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
       * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
       */
      case a @ AnnotatedType(t, _) =>
        //debuglog(s"typeKind of annotated type $a")
        toTypeRefInternal(t)
    }
  }

  /** Patches the result type of a method symbol to sanitize it.
   *
   *  For some reason, dotc thinks that the `info.resultType`of an
   *  `isConstructor` method (for classes or traits) is the enclosing class
   *  or trait, but the bodies and usages act as if the result type was `Unit`.
   *
   *  This method returns `UnitType` for constructor methods, and otherwise
   *  `sym.info.resultType`.
   */
  def patchedResultType(sym: Symbol)(using Context): Type =
    if (sym.isConstructor) defn.UnitType
    else sym.info.resultType

  def originalNameOfLocal(sym: Symbol)(
      implicit ctx: Context, localNames: LocalNameGenerator): OriginalName = {
    val irName = localNames.localSymbolName(sym)
    val originalName = UTF8String(sym.name.unexpandedName.toString)
    if (UTF8String.equals(originalName, irName.encoded)) NoOriginalName
    else OriginalName(originalName)
  }

  def originalNameOfField(sym: Symbol)(using Context): OriginalName =
    originalNameOf(sym.name)

  def originalNameOfMethod(sym: Symbol)(using Context): OriginalName =
    originalNameOf(sym.name)

  def originalNameOfClass(sym: Symbol)(using Context): OriginalName =
    originalNameOf(sym.fullName)

  private def originalNameOf(name: Name): OriginalName = {
    val originalName = name.unexpandedName.toString
    if (originalName == name.mangledString) NoOriginalName
    else OriginalName(originalName)
  }
}
