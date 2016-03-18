package dotty.tools.backend.sjs

import scala.collection.mutable

import dotty.tools.FatalError

import dotty.tools.dotc.core._
import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import NameOps._
import StdNames._

import org.scalajs.core.ir
import ir.{Trees => js, Types => jstpe}

import ScopedVar.withScopedVars
import JSDefinitions._
import JSInterop._

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

  /** Signature separator string (between parameter types) */
  private final val SignatureSep = "__"

  /** Name given to the local Scala.js environment variable */
  private final val ScalaJSEnvironmentName = "ScalaJS"

  implicit class SymOps(val self: Symbol) extends AnyVal {
    def unexpandedName(implicit ctx: Context): Names.Name =
      self.name.unexpandedName
  }

  implicit class MyNameOps(val self: Names.Name) extends AnyVal {
    def decoded: String = self.decode.toString
  }

  // Fresh local name generator ----------------------------------------------

  class LocalNameGenerator {
    import LocalNameGenerator._

    private val usedLocalNames = mutable.Set.empty[String]
    private val localSymbolNames = mutable.Map.empty[Symbol, String]

    def localSymbolName(sym: Symbol)(implicit ctx: Context): String =
      localSymbolNames.getOrElseUpdate(sym, freshName(sym.name.toString))

    def freshLocalIdent()(implicit pos: ir.Position): js.Ident =
      js.Ident(freshName(), None)

    def freshLocalIdent(base: String)(implicit pos: ir.Position): js.Ident =
      js.Ident(freshName(base), Some(base))

    private def freshName(base: String = "x"): String = {
      var suffix = 1
      var longName = base
      while (usedLocalNames(longName) || isReserved(longName)) {
        suffix += 1
        longName = base+"$"+suffix
      }
      usedLocalNames += longName
      mangleJSName(longName)
    }
  }

  private object LocalNameGenerator {
    private val isReserved =
      Set("arguments", "eval", ScalaJSEnvironmentName)
  }

  // Encoding methods ----------------------------------------------------------

  def encodeLabelSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position, localNames: LocalNameGenerator): js.Ident = {
    require(sym.is(Flags.Label), "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident(localNames.localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  private def allRefClasses(implicit ctx: Context): Set[Symbol] = {
    //TODO
    /*(Set(ObjectRefClass, VolatileObjectRefClass) ++
        refClass.values ++ volatileRefClass.values)*/
    Set()
  }

  def encodeFieldSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.is(Flags.Method) && !sym.is(Flags.Module),
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = encodeMemberNameInternal(sym)
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    /* We have to special-case fields of Ref types (IntRef, ObjectRef, etc.)
     * because they are emitted as private by our .scala source files, but
     * they are considered public at use site since their symbols come from
     * Java-emitted .class files.
     */
    val idSuffix =
      if (sym.is(Flags.Private) || allRefClasses.contains(sym.owner))
        sym.owner.asClass.baseClasses.size.toString
      else
        "f"

    val encodedName = name + "$" + idSuffix
    js.Ident(mangleJSName(encodedName), Some(sym.unexpandedName.decoded))
  }

  def encodeMethodSym(sym: Symbol, reflProxy: Boolean = false)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))
  }

  def encodeMethodName(sym: Symbol, reflProxy: Boolean = false)(
      implicit ctx: Context): String = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    encodedName + paramsString
  }

  /** Encodes a method symbol of java.lang.String for use in RuntimeString.
   *
   *  This basically means adding an initial parameter of type
   *  java.lang.String, which is the `this` parameter.
   */
  def encodeRTStringMethodSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    require(sym.owner == defn.StringClass)
    require(!sym.isClassConstructor && !sym.is(Flags.Private))

    val (encodedName, paramsString) =
      encodeMethodNameInternal(sym, inRTClass = true)
    js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))
  }

  /** Encodes a constructor symbol of java.lang.String for use in RuntimeString.
   *
   *  - The name is rerouted to `newString`
   *  - The result type is set to `java.lang.String`
   */
  def encodeRTStringCtorSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    require(sym.owner == defn.StringClass)
    require(sym.isClassConstructor && !sym.is(Flags.Private))

    val paramTypeNames = sym.info.firstParamTypes.map(internalName(_))
    val paramAndResultTypeNames = paramTypeNames :+ ir.Definitions.StringClass
    val paramsString = makeParamsString(paramAndResultTypeNames)

    js.Ident("newString" + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))
  }

  private def encodeMethodNameInternal(sym: Symbol,
      reflProxy: Boolean = false, inRTClass: Boolean = false)(
      implicit ctx: Context): (String, String) = {
    require(sym.is(Flags.Method), "encodeMethodSym called with non-method symbol: " + sym)

    def name = encodeMemberNameInternal(sym)

    val encodedName = {
      if (sym.isClassConstructor) {
        "init_"
      } else if (sym.is(Flags.Private)) {
        (mangleJSName(name) + SignatureSep + "p" +
            sym.owner.asClass.baseClasses.size.toString)
      } else {
        mangleJSName(name)
      }
    }

    val paramsString = makeParamsString(sym, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  def encodeStaticMemberSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    require(sym.is(Flags.JavaStaticTerm),
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.Ident(
        mangleJSName(encodeMemberNameInternal(sym)) +
        makeParamsString(List(internalName(sym.info))),
        Some(sym.unexpandedName.decoded))
  }

  def encodeLocalSym(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position, localNames: LocalNameGenerator): js.Ident = {
    require(!sym.owner.isClass && sym.isTerm && !sym.is(Flags.Method) && !sym.is(Flags.Module),
        "encodeLocalSym called with non-local symbol: " + sym)
    js.Ident(localNames.localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  def foreignIsImplClass(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.name.isImplClassName

  def encodeClassType(sym: Symbol)(implicit ctx: Context): jstpe.Type = {
    if (sym == defn.ObjectClass) jstpe.AnyType
    else if (isJSType(sym)) jstpe.AnyType
    else {
      assert(sym != defn.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassFullName(sym))
    }
  }

  def encodeClassFullNameIdent(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName.toString))
  }

  def encodeClassFullName(sym: Symbol)(implicit ctx: Context): String = {
    if (sym == defn.NothingClass) ir.Definitions.RuntimeNothingClass
    else if (sym == defn.NullClass) ir.Definitions.RuntimeNullClass
    else ir.Definitions.encodeClassName(sym.fullName.toString)
  }

  private def encodeMemberNameInternal(sym: Symbol)(
      implicit ctx: Context): String = {
    sym.name.toString.replace("_", "$und").replace("~", "$tilde")
  }

  def toIRType(tp: Type)(implicit ctx: Context): jstpe.Type = {
    val refType = toReferenceTypeInternal(tp)
    refType._1 match {
      case tpe: jstpe.ClassType =>
        val sym = refType._2
        if (sym.asClass.isPrimitiveValueClass) {
          if (sym == defn.BooleanClass)
            jstpe.BooleanType
          else if (sym == defn.FloatClass)
            jstpe.FloatType
          else if (sym == defn.DoubleClass)
            jstpe.DoubleType
          else if (sym == defn.LongClass)
            jstpe.LongType
          else if (sym == defn.UnitClass)
            jstpe.NoType
          else
            jstpe.IntType
        } else {
          if (sym == defn.ObjectClass || isJSType(sym))
            jstpe.AnyType
          else if (sym == defn.NothingClass)
            jstpe.NothingType
          else if (sym == defn.NullClass)
            jstpe.NullType
          else
            tpe
        }

      case tpe: jstpe.ArrayType =>
        tpe
    }
  }

  def toReferenceType(tp: Type)(implicit ctx: Context): jstpe.ReferenceType =
    toReferenceTypeInternal(tp)._1

  private def toReferenceTypeInternal(tp: Type)(
      implicit ctx: Context): (jstpe.ReferenceType, Symbol) = {

    /**
     * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
     * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
     */
    def primitiveOrClassToRefType(sym: Symbol): (jstpe.ReferenceType, Symbol) = {
      assert(sym.isClass, sym)
      //assert(sym != defn.ArrayClass || isCompilingArray, sym)
      (jstpe.ClassType(encodeClassFullName(sym)), sym)
    }

    /**
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
     */
    def nonClassTypeRefToRefType(sym: Symbol): (jstpe.ReferenceType, Symbol) = {
      //assert(sym.isType && isCompilingArray, sym)
      (jstpe.ClassType(ir.Definitions.ObjectClass), defn.ObjectClass)
    }

    tp.widenDealias match {
      // Array type such as Array[Int] (kept by erasure)
      case JavaArrayType(el) =>
        val elRefType = toReferenceTypeInternal(el)
        (jstpe.ArrayType(elRefType._1), elRefType._2)

      case t: TypeRef =>
        if (!t.symbol.isClass) nonClassTypeRefToRefType(t.symbol)  // See comment on nonClassTypeRefToBType
        else primitiveOrClassToRefType(t.symbol) // Common reference to a type such as scala.Int or java.lang.String

      case Types.ClassInfo(_, sym, _, _, _) =>
        /* We get here, for example, for genLoadModule, which invokes
         * toTypeKind(moduleClassSymbol.info)
         */
        primitiveOrClassToRefType(sym)

      case t: MethodType => // triggers for LabelDefs
        toReferenceTypeInternal(t.resultType)

      /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
       * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
       * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
       */
      case a @ AnnotatedType(t, _) =>
        //debuglog(s"typeKind of annotated type $a")
        toReferenceTypeInternal(t)
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
  def patchedResultType(sym: Symbol)(implicit ctx: Context): Type =
    if (sym.isConstructor) defn.UnitType
    else sym.info.resultType

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol, reflProxy: Boolean,
      inRTClass: Boolean)(
      implicit ctx: Context): String = {
    val tpe = sym.info

    val paramTypeNames0 = tpe.firstParamTypes.map(internalName(_))

    val hasExplicitThisParameter =
      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    val paramTypeNames =
      if (!hasExplicitThisParameter) paramTypeNames0
      else encodeClassFullName(sym.owner) :: paramTypeNames0

    val paramAndResultTypeNames = {
      if (sym.isClassConstructor)
        paramTypeNames
      else if (reflProxy)
        paramTypeNames :+ ""
      else
        paramTypeNames :+ internalName(patchedResultType(sym))
    }
    makeParamsString(paramAndResultTypeNames)
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(SignatureSep, SignatureSep, "")

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type)(implicit ctx: Context): String =
    encodeReferenceType(toReferenceType(tpe))

  /** Encodes a [[Types.ReferenceType]], such as in an encoded method signature.
   */
  private def encodeReferenceType(refType: jstpe.ReferenceType): String = {
    refType match {
      case jstpe.ClassType(encodedName) => encodedName
      case jstpe.ArrayType(base, depth) => "A" * depth + base
    }
  }

  /** Mangles names that are illegal in JavaScript by prepending a `$`.
   *  Also mangles names that would collide with these mangled names.
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$') "$" + name
    else name
}
