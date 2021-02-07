package scala.runtime.stdLibPatches

/** Scala 3 additions and replacements to the `scala.language` object.
 */
object language:

  /** The experimental object contains features that have been recently added but have not
   *  been thoroughly tested in production yet.
   *
   *  Experimental features '''may undergo API changes''' in future releases, so production
   *  code should not rely on them.
   *
   *  Programmers are encouraged to try out experimental features and
   *  [[http://issues.scala-lang.org report any bugs or API inconsistencies]]
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
      * @see [[https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs]]
      */
    object namedTypeArguments

    /** Experimental support for generic number literals.
      *
      * @see [[https://dotty.epfl.ch/docs/reference/changed-features/numeric-literals]]
      */
    object genericNumberLiterals
  end experimental

  /** Where imported, auto-tupling is disabled.
    *
    * '''Why control the feature?''' Auto-tupling can lead to confusing and
    * brittle code in presence of overloads. In particular, surprising overloads
    * can be selected, and adding new overloads can change which overload is selected
    * in suprising ways.
    *
    * '''Why allow it?''' Not allowing auto-tupling is difficult to reconcile with
    * operators accepting tuples.
    */
  object noAutoTupling

  /** Where imported, loose equality using eqAny is disabled.
    *
    * '''Why allow and control the feature?''' For compatibility and migration reasons,
    * strict equality is opt-in. See linked documentation for more information.
    *
    * @see [[https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality]]
    */
  object strictEquality

  /** Where imported, ad hoc extensions of non-open classes in other
   *  compilation units are allowed.
   *
   *  '''Why control the feature?''' Ad-hoc extensions should usually be avoided
   *  since they typically cannot rely on an "internal" contract between a class
   *  and its extensions. Only open classes need to specify such a contract.
   *  Ad-hoc extensions might break for future versions of the extended class,
   *  since the extended class is free to change its implementation without
   *  being constrained by an internal contract.
   *
   *  '''Why allow it?''' An ad-hoc extension can sometimes be necessary,
   *  for instance when mocking a class in a testing framework, or to work
   *  around a bug or missing feature in the original class. Nevertheless,
   *  such extensions should be limited in scope and clearly documented.
   *  That's why the language import is required for them.
   */
  object adhocExtensions

  object unsafeNulls

  /** Set source version to 3.0-migration.
    *
    * @see [[https://scalacenter.github.io/scala-3-migration-guide/docs/scala-3-migration-mode]]
    */
  object `3.0-migration`

  /** Set source version to 3.0.
    *
    * @see [[https://scalacenter.github.io/scala-3-migration-guide/docs/scala-3-migration-mode]]
    */
  object `3.0`

  /** Set source version to 3.1-migration.
    *
    * @see [[https://scalacenter.github.io/scala-3-migration-guide/docs/scala-3-migration-mode]]
    */
  object `3.1-migration`

  /** Set source version to 3.1
    *
    * @see [[https://scalacenter.github.io/scala-3-migration-guide/docs/scala-3-migration-mode]]
    */
  object `3.1`
end language
