/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.language.`2.13`

import scala.annotation.compileTimeOnly

/**
 *  The `scala.language` object controls the language features available to the programmer, as proposed in the
 *  [**SIP-18 document**](https://docs.google.com/document/d/1nlkvpoIRkx7at1qJEZafJwthZ3GeIklTFhqmXMvTX9Q/edit).
 *
 *  Each of these features has to be explicitly imported into the current scope to become available:
 *  ```
 *     import language.postfixOps // or language._
 *     List(1, 2, 3) reverse
 *  ```
 *
 *  The language features are:
 *   - [[dynamics            `dynamics`]]            enables defining calls rewriting using the [[scala.Dynamic `Dynamic`]] trait
 *   - [[existentials        `existentials`]]        enables writing existential types
 *   - [[higherKinds         `higherKinds`]]         enables writing higher-kinded types
 *   - [[implicitConversions `implicitConversions`]] enables defining implicit methods and members
 *   - [[postfixOps          `postfixOps`]]          enables postfix operators (not recommended)
 *   - [[reflectiveCalls     `reflectiveCalls`]]     enables using structural types
 *   - [[experimental        `experimental`]]        contains newer features that have not yet been tested in production
 *
 *  @groupname production   Language Features
 *  @groupname experimental Experimental Language Features
 *  @groupprio experimental 10

 */
object language {

  import languageFeature._

  /** Only where this feature is enabled, can direct or indirect subclasses of trait scala.Dynamic
   *  be defined. If `dynamics` is not enabled, a definition of a class, trait,
   *  or object that has `Dynamic` as a base trait is rejected by the compiler.
   *
   *  Selections of dynamic members of existing subclasses of trait `Dynamic` are unaffected;
   *  they can be used anywhere.
   *
   *  **Why introduce the feature?** To enable flexible DSLs and convenient interfacing
   *  with dynamic languages.
   *
   *  **Why control it?** Dynamic member selection can undermine static checkability
   *  of programs. Furthermore, dynamic member selection often relies on reflection,
   *  which is not available on all platforms.
   *
   *  @group production
   */
  implicit lazy val dynamics: dynamics = languageFeature.dynamics

  /** Only where this feature is enabled, is postfix operator notation `(expr op)` permitted.
   *  If `postfixOps` is not enabled, an expression using postfix notation is rejected by the compiler.
   *
   *  **Why keep the feature?** Postfix notation is preserved for backward
   *  compatibility only. Historically, several DSLs written in Scala need the notation.
   *
   *  **Why control it?** Postfix operators interact poorly with semicolon inference.
   *   Most programmers avoid them for this reason alone. Postfix syntax is
   *   associated with an abuse of infix notation, `a op1 b op2 c op3`,
   *   that can be harder to read than ordinary method invocation with judicious
   *   use of parentheses. It is recommended not to enable this feature except for
   *   legacy code.
   *
   *  @group production
   */
  implicit lazy val postfixOps: postfixOps = languageFeature.postfixOps

  /** Where this feature is enabled, accesses to members of structural types that need
   *  reflection are supported. If `reflectiveCalls` is not enabled, an expression
   *  requiring reflection will trigger a warning from the compiler.
   *
   *  A structural type is a type of the form
   *  `Parents { Decls }` where `Decls` contains declarations of new members that do
   *  not override any member in `Parents`. To access one of these members, a
   *  reflective call is needed.
   *
   *  **Why keep the feature?** Structural types provide great flexibility because
   *  they avoid the need to define inheritance hierarchies a priori. Besides,
   *  their definition falls out quite naturally from Scala’s concept of type refinement.
   *
   *  **Why control it?** Reflection is not available on all platforms. Popular tools
   *  such as ProGuard have problems dealing with it. Even where reflection is available,
   *  reflective dispatch can lead to surprising performance degradations.
   *
   *  @group production
   */
  implicit lazy val reflectiveCalls: reflectiveCalls = languageFeature.reflectiveCalls

  /** Where this feature is enabled, definitions of implicit conversion methods are allowed.
   *  If `implicitConversions` is not enabled, the definition of an implicit
   *  conversion method will trigger a warning from the compiler.
   *
   *  An implicit conversion is an implicit value of unary function type `A => B`,
   *  or an implicit method that has in its first parameter section a single,
   *  non-implicit parameter. Examples:
   *
   *  ```
   *     implicit def intToString(i: Int): String = s"\$i"
   *     implicit val conv: Int => String = i => s"\$i"
   *     implicit val numerals: List[String] = List("zero", "one", "two", "three")
   *     implicit val strlen: String => Int = _.length
   *     implicit def listToInt[T](xs: List[T])(implicit f: T => Int): Int = xs.map(f).sum
   *  ```
   *
   *  This language feature warns only for implicit conversions introduced by methods.
   *
   *  Other values, including functions or data types which extend `Function1`,
   *  such as `Map`, `Set`, and `List`, do not warn.
   *
   *  Implicit class definitions, which introduce a conversion to the wrapping class,
   *  also do not warn.
   *
   *  **Why keep the feature?** Implicit conversions are central to many aspects
   *  of Scala’s core libraries.
   *
   *  **Why control it?** Implicit conversions are known to cause many pitfalls
   *  if over-used. And there is a tendency to over-use them because they look
   *  very powerful and their effects seem to be easy to understand. Also, in
   *  most situations using implicit parameters leads to a better design than
   *  implicit conversions.
   *
   *  @group production
   */
  implicit lazy val implicitConversions: implicitConversions = languageFeature.implicitConversions

  /** Where this feature is enabled, higher-kinded types can be written.
   *  If `higherKinds` is not enabled, a higher-kinded type such as `F[A]`
   *  will trigger a warning from the compiler.
   *
   *  **Why keep the feature?** Higher-kinded types enable the definition of very general
   *  abstractions such as functor, monad, or arrow. A significant set of advanced
   *  libraries relies on them. Higher-kinded types are also at the core of the
   *  scala-virtualized effort to produce high-performance parallel DSLs through staging.
   *
   *  **Why control it?** Higher kinded types in Scala lead to a Turing-complete
   *  type system, where compiler termination is no longer guaranteed. They tend
   *  to be useful mostly for type-level computation and for highly generic design
   *  patterns. The level of abstraction implied by these design patterns is often
   *  a barrier to understanding for newcomers to a Scala codebase. Some syntactic
   *  aspects of higher-kinded types are hard to understand for the uninitiated and
   *  type inference is less effective for them than for normal types. Because we are
   *  not completely happy with them yet, it is possible that some aspects of
   *  higher-kinded types will change in future versions of Scala. So an explicit
   *  enabling also serves as a warning that code involving higher-kinded types
   *  might have to be slightly revised in the future.
   *
   *  @group production
   */
  @deprecated("higherKinds no longer needs to be imported explicitly", "2.13.1")
  implicit lazy val higherKinds: higherKinds = languageFeature.higherKinds

  /** Where this feature is enabled, existential types that cannot be expressed as wildcard
   *  types can be written and are allowed in inferred types of values or return
   *  types of methods. If `existentials` is not enabled, those cases will trigger
   *  a warning from the compiler.
   *
   *  Existential types with wildcard type syntax such as `List[?]`,
   *  or `Map[String, ?]` are not affected.
   *
   *  **Why keep the feature?** Existential types are needed to make sense of Java’s wildcard
   *  types and raw types and the erased types of run-time values.
   *
   *  **Why control it?** Having complex existential types in a code base usually makes
   *  application code very brittle, with a tendency to produce type errors with
   *  obscure error messages. Therefore, going overboard with existential types
   *  is generally perceived not to be a good idea. Also, complicated existential types
   *  might be no longer supported in a future simplification of the language.
   *
   *  @group production
   */
  implicit lazy val existentials: existentials = languageFeature.existentials

  /** The experimental object contains features that are known to have unstable API or
   *  behavior that may change in future releases.
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
  object experimental {

    import languageFeature.experimental._

    /** Only where this feature is enabled, are macro definitions allowed.
     *  If `macros` is not enabled, macro definitions are rejected by the compiler.
     *
     *  Macro implementations and macro applications are not governed by this
     *  language feature; they can be used anywhere.
     *
     *  **Why introduce the feature?** Macros promise to make the language more regular,
     *  replacing ad-hoc language constructs with a general powerful abstraction
     *  capability that can express them. Macros are also a more disciplined and
     *  powerful replacement for compiler plugins.
     *
     *  **Why control it?** For their very power, macros can lead to code that is hard
     *  to debug and understand.
     */
    implicit lazy val macros: macros = languageFeature.experimental.macros

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
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/capture-checking]]
     */
    @compileTimeOnly("`captureChecking` can only be used at compile time in import statements")
    object captureChecking

    /** Experimental support for separation checking; requires captureChecking also to be enabled.
     *
     *  @see [[https://nightly.scala-lang.org/docs/reference/experimental/capture-checking/separation-checking]]
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
  }

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
}
