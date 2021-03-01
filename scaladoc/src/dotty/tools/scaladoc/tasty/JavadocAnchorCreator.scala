package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

trait JavadocAnchorCreator:
  self: SymOps[_] =>

  import self.q.reflect._

  private val javadocPrimitivesMap = Map(
    "scala.Int" -> "int",
    "scala.Float" -> "float",
    "scala.Double" -> "double",
    "scala.Long" -> "long",
    "scala.Byte" -> "byte",
    "scala.Boolean" -> "boolean",
    "scala.Char" -> "char",
    "scala.Short" -> "short",
    "<special-ops>.<FromJavaObject>" -> "java.lang.Object"
  )

  private def transformPrimitiveType(s: String): String = javadocPrimitivesMap.getOrElse(s, s)

  private def transformType(tpe: TypeRepr): String = tpe.simplified match {
    case AppliedType(tpe, typeList) if tpe.show == "scala.Array" =>  transformType(typeList.head) + ":A"
    case AppliedType(tpe, typeList) if tpe.show == "scala.<repeated>" => transformType(typeList.head) + "..."
    case AppliedType(tpe, typeList) => transformPrimitiveType(tpe.show)
    case other => transformPrimitiveType(other.show)
  }

  def getJavadocType(s: TypeRepr) = transformType(s)
