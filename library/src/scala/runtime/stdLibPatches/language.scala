package scala.runtime.stdLibPatches

import scala.language.experimental.captureChecking

import scala.annotation.compileTimeOnly
import scala.annotation.publicInBinary

/** Scala 3 additions and replacements to the `scala.language` object. */
@publicInBinary
@deprecated(message = "Patches are not applied to the stdlib anymore", since = "3.8.0")
private[scala] object language:

  /** The experimental object contains features that have been recently added but have not
   *  been thoroughly tested in production yet.
   *
   *  Experimental features **may undergo API changes** in future releases, so production
   *  code should not rely on them.
   *
   *  Programmers are encouraged to try out experimental features and
   *  [report any bugs or API inconsistencies](https://github.com/scala/scala3/issues)
   *  they encounter so they can be improved in future releases.
   *
   *  @group experimental
   */
  object experimental:

    /* Experimental support for richer dependent types (disabled for now)
     * One can still run the compiler with support for parsing singleton applications
     * using command line option `-language:experimental.dependent`.
     * But one cannot use a feature import for this as long as this entry is commented out.
     */
    //object dependent

    /** Experimental support for named type arguments.
      *
      * @see [[https://nightly.scala-lang.org/docs/reference/other-new-features/named-typeargs]]
      */
    @compileTimeOnly("`namedTypeArguments` can only be used at compile time in import statements")
    object namedTypeArguments

    /** Experimental support for generic number literals.
      *
      * @see [[https://nightly.scala-lang.org/docs/reference/changed-features/numeric-literals]]
      */
    @compileTimeOnly("`genericNumberLiterals` can only be used at compile time in import statements")
    object genericNumberLiterals

    /** Experimental support for `erased` modifier
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/erased-defs]]
     */
    @compileTimeOnly("`erasedDefinitions` can only be used at compile time in import statements")
    object erasedDefinitions

    /** Experimental support for relaxed CanEqual checks for ADT pattern matching
     *
     * @see [[https://github.com/scala/improvement-proposals/pull/97]]
     */
    @compileTimeOnly("`strictEqualityPatternMatching` can only be used at compile time in import statements")
    object strictEqualityPatternMatching

    /** Experimental support for using indentation for arguments
     */
    @compileTimeOnly("`fewerBraces` can only be used at compile time in import statements")
    @deprecated("`fewerBraces` is now standard, no language import is needed", since = "3.3")
    object fewerBraces

    /** Experimental support for typechecked exception capabilities
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/canthrow]]
     */
    @compileTimeOnly("`saferExceptions` can only be used at compile time in import statements")
    object saferExceptions

    /** Adds support for clause interleaving:
      * Methods can now have as many type clauses as they like, this allows to have type bounds depend on terms: `def f(x: Int)[A <: x.type]: A`
      *
      * @see [[https://github.com/scala/improvement-proposals/blob/main/content/clause-interleaving.md]]
      */
    @compileTimeOnly("`clauseInterleaving` can only be used at compile time in import statements")
    @deprecated("`clauseInterleaving` is now standard, no language import is needed", since = "3.6")
    object clauseInterleaving

    /** Experimental support for pure function type syntax
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/purefuns]]
     */
    @compileTimeOnly("`pureFunctions` can only be used at compile time in import statements")
    object pureFunctions

    /** Experimental support for capture checking; implies support for pureFunctions
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/cc]]
     */
    @compileTimeOnly("`captureChecking` can only be used at compile time in import statements")
    object captureChecking

    /** Experimental support for separation checking; requires captureChecking also to be enabled.
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/cc]]
     */
    @compileTimeOnly("`separationChecking` can only be used at compile time in import statements")
    object separationChecking

    /** Experimental support for automatic conversions of arguments, without requiring
     *  a language import `import scala.language.implicitConversions`.
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/into-modifier]]
     */
    @compileTimeOnly("`into` can only be used at compile time in import statements")
    @deprecated("The into language import is no longer needed since the feature is now in preview", since = "3.8")
    object into

    /** Experimental support for named tuples.
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/named-tuples]]
     */
    @compileTimeOnly("`namedTuples` can only be used at compile time in import statements")
    @deprecated("The experimental.namedTuples language import is no longer needed since the feature is now standard", since = "3.7")
    object namedTuples

    /** Experimental support for new features for better modularity, including
     *   - better tracking of dependencies through classes
     *   - better usability of context bounds
     *   - better syntax and conventions for type classes
     *   - ability to merge exported types in intersections
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/modularity]]
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/typeclasses]]
     */
    @compileTimeOnly("`modularity` can only be used at compile time in import statements")
    object modularity

    /** Was needed to add support for relaxed imports of extension methods.
      * The language import is no longer needed as this is now a standard feature since SIP was accepted.
      * @see [[https://nightly.scala-lang.org/docs/reference/contextual/extension-methods]]
      */
    @compileTimeOnly("`relaxedExtensionImports` can only be used at compile time in import statements")
    @deprecated("The experimental.relaxedExtensionImports language import is no longer needed since the feature is now standard", since = "3.4")
    object relaxedExtensionImports

    /** Enhance match type extractors to follow aliases and singletons.
     *
     *  @see [[https://github.com/scala/improvement-proposals/pull/84]]
     */
    @compileTimeOnly("`betterMatchTypeExtractors` can only be used at compile time in import statements")
    @deprecated("The experimental.betterMatchTypeExtractors language import is no longer needed since the feature is now standard. It now has no effect, including when setting an older source version.", since = "3.6")
    object betterMatchTypeExtractors

    /** Experimental support for quote pattern matching with polymorphic functions
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/quoted-patterns-with-polymorphic-functions]]
     */
    @compileTimeOnly("`quotedPatternsWithPolymorphicFunctions` can only be used at compile time in import statements")
    object quotedPatternsWithPolymorphicFunctions

    /** Experimental support for improvements in `for` comprehensions
     *
     * @see [[https://github.com/scala/improvement-proposals/pull/79]]
     */
    @compileTimeOnly("`betterFors` can only be used at compile time in import statements")
    @deprecated("The `experimental.betterFors` language import no longer has any effect, the feature is being stabilised and can be enabled using `-preview` flag", since = "3.7")
    object betterFors

    /** Experimental support for package object values
     */
    @compileTimeOnly("`packageObjectValues` can only be used at compile time in import statements")
    object packageObjectValues

    /** Experimental support for multiple spread arguments.
     */
    @compileTimeOnly("`multiSpreads` can only be used at compile time in import statements")
    object multiSpreads

    /** Experimental support for match expressions with sub cases.
     */
    @compileTimeOnly("`subCases` can only be used at compile time in import statements")
    object subCases

    /** Experimental support for single-line lambdas and case clause expressions after `:`
     */
    @compileTimeOnly("`relaxedLambdaSyntax` can only be used at compile time in import statements")
    object relaxedLambdaSyntax
  end experimental

  /** The deprecated object contains features that are no longer officially suypported in Scala.
   *  Features in this object are slated for removal. New code should not use them and
   *  old code should migrate away from them.
   */
  @compileTimeOnly("`deprecated` can only be used at compile time in import statements")
  object deprecated:

    /** Symbol literals have been deprecated since 2.13. Since Scala 3.0 they
     *  are no longer an official part of Scala. For compatibility with legacy software,
     *  symbol literals are still supported with a language import, but new software
     *  should not use them.
     */
    @compileTimeOnly("`symbolLiterals` can only be used at compile time in import statements")
    object symbolLiterals

  end deprecated

  /** Where imported, auto-tupling is disabled.
    *
    * **Why control the feature?** Auto-tupling can lead to confusing and
    * brittle code in presence of overloads. In particular, surprising overloads
    * can be selected, and adding new overloads can change which overload is selected
    * in suprising ways.
    *
    * **Why allow it?** Not allowing auto-tupling is difficult to reconcile with
    * operators accepting tuples.
    */
  @compileTimeOnly("`noAutoTupling` can only be used at compile time in import statements")
  object noAutoTupling

  /** Where imported, loose equality using eqAny is disabled.
    *
    * **Why allow and control the feature?** For compatibility and migration reasons,
    * strict equality is opt-in. See linked documentation for more information.
    *
    * @see [[https://nightly.scala-lang.org/docs/reference/contextual/multiversal-equality]]
    */
  @compileTimeOnly("`strictEquality` can only be used at compile time in import statements")
  object strictEquality

  /** Where imported, ad hoc extensions of non-open classes in other
   *  compilation units are allowed.
   *
   *  **Why control the feature?** Ad-hoc extensions should usually be avoided
   *  since they typically cannot rely on an "internal" contract between a class
   *  and its extensions. Only open classes need to specify such a contract.
   *  Ad-hoc extensions might break for future versions of the extended class,
   *  since the extended class is free to change its implementation without
   *  being constrained by an internal contract.
   *
   *  **Why allow it?** An ad-hoc extension can sometimes be necessary,
   *  for instance when mocking a class in a testing framework, or to work
   *  around a bug or missing feature in the original class. Nevertheless,
   *  such extensions should be limited in scope and clearly documented.
   *  That's why the language import is required for them.
   */
  @compileTimeOnly("`adhocExtensions` can only be used at compile time in import statements")
  object adhocExtensions

  /** Unsafe Nulls fot Explicit Nulls
    * Inside the "unsafe" scope, `Null` is considered as a subtype of all reference types.
    *
    * @see [[https://nightly.scala-lang.org/docs/reference/other-new-features/explicit-nulls.html]]
    */
  @compileTimeOnly("`unsafeNulls` can only be used at compile time in import statements")
  object unsafeNulls

  @compileTimeOnly("`future` can only be used at compile time in import statements")
  object future

  @compileTimeOnly("`future-migration` can only be used at compile time in import statements")
  object `future-migration`

  /** Sets source version to 2.13. Effectively, this doesn't change the source language,
   * but rather adapts the generated code as if it was compiled with Scala 2.13
   */
  @compileTimeOnly("`2.13` can only be used at compile time in import statements")
  private[scala] object `2.13`

  /** Sets source version to 3.0-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.0-migration` can only be used at compile time in import statements")
  object `3.0-migration`

  /** Sets source version to 3.0.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.0` can only be used at compile time in import statements")
  object `3.0`

  /** Sets source version to 3.1-migration.
    *
    * This is a no-op, and should not be used. A syntax error will be reported upon import.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.1-migration` can only be used at compile time in import statements")
  @deprecated("`3.1-migration` is not valid, use `3.1` instead", since = "3.2")
  object `3.1-migration`

  /** Sets source version to 3.1
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.1` can only be used at compile time in import statements")
  object `3.1`

  /** Sets source version to 3.2-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.2-migration` can only be used at compile time in import statements")
  object `3.2-migration`

  /** Sets source version to 3.2
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.2` can only be used at compile time in import statements")
  object `3.2`

  /** Sets source version to 3.3-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.3-migration` can only be used at compile time in import statements")
  object `3.3-migration`

  /** Sets source version to 3.3
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.3` can only be used at compile time in import statements")
  object `3.3`

  /** Sets source version to 3.4-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.4-migration` can only be used at compile time in import statements")
  object `3.4-migration`

  /** Sets source version to 3.4
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.4` can only be used at compile time in import statements")
  object `3.4`

  /** Sets source version to 3.5-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.5-migration` can only be used at compile time in import statements")
  object `3.5-migration`

  /** Sets source version to 3.5
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.5` can only be used at compile time in import statements")
  object `3.5`

  /** Sets source version to 3.6-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.6-migration` can only be used at compile time in import statements")
  object `3.6-migration`

  /** Sets source version to 3.6
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.6` can only be used at compile time in import statements")
  object `3.6`

  /** Sets source version to 3.7-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.7-migration` can only be used at compile time in import statements")
  object `3.7-migration`

  /** Sets source version to 3.7
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.7` can only be used at compile time in import statements")
  object `3.7`

  /** Sets source version to 3.8-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.8-migration` can only be used at compile time in import statements")
  object `3.8-migration`

  /** Sets source version to 3.8
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.8` can only be used at compile time in import statements")
  object `3.8`

  /** Sets source version to 3.9-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.9-migration` can only be used at compile time in import statements")
  object `3.9-migration`

  /** Sets source version to 3.9
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.9` can only be used at compile time in import statements")
  object `3.9`

  // !!! Keep in sync with dotty.tools.dotc.config.SourceVersion !!!
  // Also add tests in `tests/pos/source-import-3-x.scala` and `tests/pos/source-import-3-x-migration.scala`

end language
