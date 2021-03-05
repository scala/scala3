package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

trait JavadocAnchorCreator:
  self: SymOps[_] =>

  import self.q.reflect._

  private val javadocPrimitivesMap = Map(
    defn.IntClass -> "int",
    defn.FloatClass -> "float",
    defn.DoubleClass -> "double",
    defn.LongClass -> "long",
    defn.ByteClass -> "byte",
    defn.BooleanClass -> "boolean",
    defn.CharClass -> "char",
    defn.ShortClass -> "short",
    defn.ObjectClass -> "java.lang.Object"
  )

  private def transformPrimitiveType(tpe: TypeRepr): String = tpe.classSymbol
    .flatMap(javadocPrimitivesMap.get)
    .filter(_ => !tpe.typeSymbol.isTypeParam)
    .getOrElse(tpe.show)

  private def transformType(tpe: TypeRepr): String = tpe.simplified match {
    case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.ArrayClass) => transformType(typeList.head) + ":A"
    case AppliedType(tpe, typeList) if tpe.classSymbol.fold(false)(_ == defn.RepeatedParamClass) => transformType(typeList.head) + "..."
    case AppliedType(tpe, typeList) => transformPrimitiveType(tpe)
    case other => transformPrimitiveType(other)
  }

  def getJavadocType(s: TypeRepr) = transformType(s)
