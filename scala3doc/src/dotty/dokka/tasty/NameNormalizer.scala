package dotty.dokka.tasty

import dotty.dokka._

trait NameNormalizer:
    self: TastyParser =>
    import qctx.reflect._
    extension (s: Symbol) def normalizedName: String = {
        val withoutGivenPrefix = if s.isGiven then s.name.stripPrefix("given_") else s.name
        val withoutExtensionPrefix = if s.isExtensionMethod then withoutGivenPrefix.stripPrefix("extension_") else withoutGivenPrefix
        val withoutObjectSuffix = if s.flags.is(Flags.Object) then withoutExtensionPrefix.stripSuffix("$") else withoutExtensionPrefix
        val constructorNormalizedName = if s.isClassConstructor then "this" else withoutObjectSuffix
        constructorNormalizedName
    }
