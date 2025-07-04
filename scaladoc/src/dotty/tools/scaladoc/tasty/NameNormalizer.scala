package dotty.tools.scaladoc.tasty

import dotty.tools.scaladoc._
import dotty.tools.dotc.core.StdNames.nme.keywords
import dotty.tools.dotc.core.Names.termName

import scala.quoted._
import SymOps._

object NameNormalizer {

  extension (using Quotes)(s: reflect.Symbol)
    def normalizedName: String = {
      import reflect.*
      val withoutObjectSuffix = if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
      val constructorNormalizedName = if s.isClassConstructor then "this" else withoutObjectSuffix
      val escaped = escapedName(constructorNormalizedName)
      escaped
    }

    def ownerNameChain: List[String] = {
      import reflect.*
      if s.isNoSymbol then List.empty
      else if s == defn.EmptyPackageClass then List.empty
      else if s == defn.RootPackage then List.empty
      else if s == defn.RootClass then List.empty
      else s.owner.ownerNameChain :+ s.normalizedName
    }

    def normalizedFullName: String =
      s.ownerNameChain.mkString(".")

  private val ignoredKeywords: Set[String] = Set("this")

  private def escapedName(name: String) =
    val complexIdentifierRegex = """([([{}]) ]|[^A-Za-z0-9$]_)""".r
    name match
      case n if ignoredKeywords(n) => n
      case n if keywords(termName(n)) || complexIdentifierRegex.findFirstIn(n).isDefined => s"`$n`"
      case _ => name
}
