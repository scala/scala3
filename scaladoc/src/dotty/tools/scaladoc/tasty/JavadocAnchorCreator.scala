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

  private def transformPrimitiveType(using Quotes)(tpe: reflect.TypeRepr): String = tpe.classSymbol
    .flatMap(javadocPrimitives)
    .filter(_ => !tpe.typeSymbol.isTypeParam)
    .getOrElse(tpe.show)

  private def transformType(using Quotes)(tpe: reflect.TypeRepr): String =
    import reflect.*
    tpe.simplified match
      case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.ArrayClass) => transformType(typeList.head) + ":A"
      case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.RepeatedParamClass) => transformType(typeList.head) + "..."
      case AppliedType(tpe, typeList) => transformPrimitiveType(tpe)
      case other => transformPrimitiveType(other)

  def getJavadocType(using Quotes)(s: reflect.TypeRepr) = transformType(s)
