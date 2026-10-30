package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

object JavadocAnchorCreator:

  private def javadocPrimitives(using Quotes)(sym: reflect.Symbol) =
    import reflect.*
    if sym == defn.IntClass then Some("int")
    else if sym == defn.FloatClass then Some("float")
    else if sym == defn.DoubleClass then Some("double")
    else if sym == defn.LongClass then Some("long")
    else if sym == defn.ByteClass then Some("byte")
    else if sym == defn.BooleanClass then Some("boolean")
    else if sym == defn.CharClass then Some("char")
    else if sym == defn.ShortClass then Some("short")
    else if sym == defn.ObjectClass then Some("java.lang.Object")
    else None

  // A static nested class is a member of the companion module class, e.g. `Thread$`
  private def javadocName(using Quotes)(sym: reflect.Symbol): String =
    import reflect.*
    val owner = sym.owner
    if owner.isPackageDef then sym.fullName
    else
      val companion = if owner.flags.is(Flags.Module) then owner.companionClass else Symbol.noSymbol
      javadocName(if companion.exists then companion else owner) + "." + sym.name

  // javadoc names type parameters as declared (`add(E)`, not `ArrayList.this.E`), and
  // nested classes with dots (`java.lang.Thread.UncaughtExceptionHandler`)
  private def transformPrimitiveType(using Quotes)(tpe: reflect.TypeRepr): String =
    if tpe.typeSymbol.isTypeParam then tpe.typeSymbol.name
    else tpe.classSymbol.map(sym => javadocPrimitives(sym).getOrElse(javadocName(sym))).getOrElse(tpe.show)

  private def transformType(using Quotes)(tpe: reflect.TypeRepr): String =
    import reflect.*
    tpe.simplified match
      case FlexibleType(tpe) => transformType(tpe)
      case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.ArrayClass) => transformType(typeList.head) + "[]"
      case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.RepeatedParamClass) => transformType(typeList.head) + "..."
      case AppliedType(tpe, typeList) => transformPrimitiveType(tpe)
      case other => transformPrimitiveType(other)

  def getJavadocType(using Quotes)(s: reflect.TypeRepr) = transformType(s)
