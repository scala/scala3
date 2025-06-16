package dotty.tools.debug

import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import dotty.tools.dotc.util.NameTransformer

// Inspired by https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/backend/sjs/JSEncoding.scala
private object JavaEncoding:
  def encode(tpe: Type)(using Context): String =
    tpe.widenDealias match
      // Array type such as Array[Int] (kept by erasure)
      case JavaArrayType(el) => s"[${binaryName(el)}"
      case tpe: TypeRef => encode(tpe.symbol.asType)
      case AnnotatedType(t, _) => encode(t)

  def encode(sym: TypeSymbol)(using Context): String =
    /* When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TypeRef to T is replaced by ObjectReference.
     */
    if !sym.isClass then "java.lang.Object"
    else if sym.isPrimitiveValueClass then primitiveName(sym)
    else className(sym)

  def encode(name: TermName)(using Context): String =
    NameTransformer.encode(name.toSimpleName).toString

  private def binaryName(tpe: Type)(using Context): String =
    tpe match
      case JavaArrayType(el) => s"[${binaryName(el)}"
      case tpe: TypeRef =>
        if tpe.symbol.isPrimitiveValueClass then primitiveBinaryName(tpe.symbol)
        else classBinaryName(tpe.symbol)
      case AnnotatedType(t, _) => binaryName(t)

  private def primitiveName(sym: Symbol)(using Context): String =
    if sym == defn.UnitClass then "void"
    else if sym == defn.BooleanClass then "boolean"
    else if sym == defn.CharClass then "char"
    else if sym == defn.ByteClass then "byte"
    else if sym == defn.ShortClass then "short"
    else if sym == defn.IntClass then "int"
    else if sym == defn.LongClass then "long"
    else if sym == defn.FloatClass then "float"
    else if sym == defn.DoubleClass then "double"
    else throw new Exception(s"Unknown primitive value class $sym")

  private def primitiveBinaryName(sym: Symbol)(using Context): String =
    if sym == defn.BooleanClass then "Z"
    else if sym == defn.CharClass then "C"
    else if sym == defn.ByteClass then "B"
    else if sym == defn.ShortClass then "S"
    else if sym == defn.IntClass then "I"
    else if sym == defn.LongClass then "J"
    else if sym == defn.FloatClass then "F"
    else if sym == defn.DoubleClass then "D"
    else throw new Exception(s"Unknown primitive value class $sym")

  private def className(sym: Symbol)(using Context): String =
    val sym1 =
      if (sym.isAllOf(ModuleClass | JavaDefined)) sym.linkedClass
      else sym

    /* Some re-wirings:
     * - scala.Nothing to scala.runtime.Nothing$.
     * - scala.Null to scala.runtime.Null$.
     */
    if sym1 == defn.NothingClass then "scala.runtime.Nothing$"
    else if sym1 == defn.NullClass then "scala.runtime.Null$"
    else sym1.javaClassName

  private def classBinaryName(sym: Symbol)(using Context): String =
    s"L${className(sym)};"
