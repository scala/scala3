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
import Names._
import StdNames._

import org.scalajs.ir
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
    def unexpandedName(implicit ctx: Context): Name =
      self.name.unexpandedName
  }

  implicit class MyNameOps(val self: Name) extends AnyVal {
    def decoded: String = self.decode.toString
  }

  // Fresh local name generator ----------------------------------------------

  class LocalNameGenerator {
    import LocalNameGenerator._

    private val usedLocalNames = mutable.Set.empty[String]
    private val localSymbolNames = mutable.Map.empty[Symbol, String]
    private var returnLabelName: Option[String] = None

    def localSymbolName(sym: Symbol)(implicit ctx: Context): String =
      localSymbolNames.getOrElseUpdate(sym, freshName(sym.name.toString))

    def freshLocalIdent()(implicit pos: ir.Position): js.Ident =
      js.Ident(freshName(), None)

    def freshLocalIdent(base: String)(implicit pos: ir.Position): js.Ident =
      js.Ident(freshName(base), Some(base))

    def freshLocalIdent(base: TermName)(implicit pos: ir.Position): js.Ident =
      js.Ident(freshName(base.toString), Some(base.unexpandedName.toString))

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

    def getEnclosingReturnLabel()(implicit pos: ir.Position): js.Ident = {
      /*val box = returnLabelName.get
      if (box == null)
        throw new IllegalStateException(s"No enclosing returnable scope at $pos")*/
      if (returnLabelName.isEmpty)
        returnLabelName = Some(freshName("_return"))
      js.Ident(returnLabelName.get)
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

  private def encodeMethodNameInternal(sym: Symbol,
      reflProxy: Boolean = false)(
      implicit ctx: Context): (String, String) = {
    require(sym.is(Flags.Method), "encodeMethodSym called with non-method symbol: " + sym)

    def name = encodeMemberNameInternal(sym)

    val encodedName =
      if (sym.isClassConstructor) "init_"
      else mangleJSName(name)

    val paramsString = makeParamsString(sym, reflProxy)

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

  def encodeClassType(sym: Symbol)(implicit ctx: Context): jstpe.Type = {
    if (sym == defn.ObjectClass) jstpe.AnyType
    else if (isJSType(sym)) jstpe.AnyType
    else {
      assert(sym != defn.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassFullName(sym))
    }
  }

  def encodeClassRef(sym: Symbol)(implicit ctx: Context): jstpe.ClassRef =
    jstpe.ClassRef(encodeClassFullName(sym))

  def encodeClassFullNameIdent(sym: Symbol)(
      implicit ctx: Context, pos: ir.Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName.toString))
  }

  def encodeClassFullName(sym: Symbol)(implicit ctx: Context): String = {
    if (sym == defn.BoxedUnitClass) {
      /* Rewire scala.runtime.BoxedUnit to java.lang.Void, as the IR expects.
       * BoxedUnit$ is a JVM artifact.
       */
      ir.Definitions.BoxedUnitClass
    } else {
      ir.Definitions.encodeClassName(fullyMangledString(sym.fullName))
    }
  }

  private def encodeMemberNameInternal(sym: Symbol)(
      implicit ctx: Context): String = {
    fullyMangledString(sym.name)
  }

  /** Work around https://github.com/lampepfl/dotty/issues/5936 by bridging
   *  most (all?) of the gap in encoding so that Dotty.js artifacts are
   *  compatible with the restrictions on valid IR identifier names.
   */
  private def fullyMangledString(name: Name): String = {
    val base = name.mangledString
    val len = base.length

    // slow path
    def encodeFurther(): String = {
      val result = new java.lang.StringBuilder()
      var i = 0
      while (i != len) {
        val c = base.charAt(i)
        if (c == '_')
          result.append("$und")
        else if (Character.isJavaIdentifierPart(c) || c == '.')
          result.append(c)
        else
          result.append("$u%04x".format(c.toInt))
        i += 1
      }
      result.toString()
    }

    var i = 0
    while (i != len) {
      val c = base.charAt(i)
      if (c == '_' || !Character.isJavaIdentifierPart(c))
        return encodeFurther()
      i += 1
    }
    base
  }

  def toIRType(tp: Type)(implicit ctx: Context): jstpe.Type = {
    val typeRefInternal = toTypeRefInternal(tp)
    typeRefInternal._1 match {
      case typeRef: jstpe.ClassRef =>
        val sym = typeRefInternal._2
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
            jstpe.ClassType(typeRef.className)
        }

      case typeRef: jstpe.ArrayTypeRef =>
        jstpe.ArrayType(typeRef)
    }
  }

  def toTypeRef(tp: Type)(implicit ctx: Context): jstpe.TypeRef =
    toTypeRefInternal(tp)._1

  private def toTypeRefInternal(tp: Type)(implicit ctx: Context): (jstpe.TypeRef, Symbol) = {
    /**
     * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
     * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
     */
    def primitiveOrClassToTypeRef(sym: Symbol): (jstpe.TypeRef, Symbol) = {
      assert(sym.isClass, sym)
      //assert(sym != defn.ArrayClass || isCompilingArray, sym)
      val className = if (sym.isPrimitiveValueClass) {
        if (sym == defn.UnitClass) ir.Definitions.VoidClass
        else if (sym == defn.BooleanClass) ir.Definitions.BooleanClass
        else if (sym == defn.CharClass) ir.Definitions.CharClass
        else if (sym == defn.ByteClass) ir.Definitions.ByteClass
        else if (sym == defn.ShortClass) ir.Definitions.ShortClass
        else if (sym == defn.IntClass) ir.Definitions.IntClass
        else if (sym == defn.LongClass) ir.Definitions.LongClass
        else if (sym == defn.FloatClass) ir.Definitions.FloatClass
        else if (sym == defn.DoubleClass) ir.Definitions.DoubleClass
        else throw new Exception(s"unknown primitive value class $sym")
      } else {
        encodeClassFullName(sym)
      }
      (jstpe.ClassRef(className), sym)
    }

    /**
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
     */
    def nonClassTypeRefToTypeRef(sym: Symbol): (jstpe.TypeRef, Symbol) = {
      //assert(sym.isType && isCompilingArray, sym)
      (jstpe.ClassRef(ir.Definitions.ObjectClass), defn.ObjectClass)
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
  def patchedResultType(sym: Symbol)(implicit ctx: Context): Type =
    if (sym.isConstructor) defn.UnitType
    else sym.info.resultType

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol, reflProxy: Boolean)(
      implicit ctx: Context): String = {
    val tpe = sym.info

    val paramTypeNames0 = tpe.firstParamTypes.map(internalName(_))

    val hasExplicitThisParameter = isScalaJSDefinedJSClass(sym.owner)
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
    encodeTypeRef(toTypeRef(tpe))

  /** Encodes a [[Types.TypeRef]], such as in an encoded method signature.
   */
  private def encodeTypeRef(typeRef: jstpe.TypeRef): String = {
    typeRef match {
      case jstpe.ClassRef(className) => className
      case jstpe.ArrayTypeRef(base, depth) => "A" * depth + base
    }
  }

  /** Mangles names that are illegal in JavaScript by prepending a `$`.
   *  Also mangles names that would collide with these mangled names.
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$') "$" + name
    else name
}
