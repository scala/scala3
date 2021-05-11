package dotty.tools.scaladoc.tasty

import dotty.tools.scaladoc._
import dotty.tools.dotc.core.StdNames.nme.keywords
import dotty.tools.dotc.core.Names.termName

import scala.quoted._
import SymOps._

object NameNormalizer {

  extension (using Quotes)(s: reflect.Symbol) def normalizedName: String = {
    import reflect.*
    val withoutGivenPrefix = if s.isGiven then s.name.stripPrefix("given_") else s.name
    val withoutObjectSuffix = if s.flags.is(Flags.Module) then withoutGivenPrefix.stripSuffix("$") else withoutGivenPrefix
    val constructorNormalizedName = if s.isClassConstructor then "this" else withoutObjectSuffix
    val escaped = escapedName(constructorNormalizedName)
    escaped
  }

  private val ignoredKeywords: Set[String] = Set("this")

  private def escapedName(name: String) =
    val complexIdentifierRegex = """([([{}]) ]|[^A-Za-z0-9$]_)""".r
    name match
      case n if ignoredKeywords(n) => n
      case n if keywords(termName(n)) || complexIdentifierRegex.findFirstIn(n).isDefined => s"`$n`"
      case _ => name
}
