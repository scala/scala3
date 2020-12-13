package dotty.dokka.tasty

import dotty.dokka._

trait NameNormalizer { self: TastyParser =>
  import qctx.reflect._
  extension (s: Symbol) def normalizedName: String = {
    val withoutGivenPrefix = if s.isGiven then s.name.stripPrefix("given_") else s.name
    val withoutObjectSuffix = if s.flags.is(Flags.Module) then withoutGivenPrefix.stripSuffix("$") else withoutGivenPrefix
    val constructorNormalizedName = if s.isClassConstructor then "this" else withoutObjectSuffix
    constructorNormalizedName
  }
}
