package scala.runtime.stdLibPatches

/** Scala 3 additions to the `scala.language` object.
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

    /** Experimental support for richer dependent types */
    object dependent

    /** Experimental support for named type arguments */
    object namedTypeArguments

    /** Experimental support for generic number literals */
    object genericNumberLiterals
  end experimental

  /** Where imported, auto-tupling is disabled */
  object noAutoTupling

  /** Where imported, loose equality using eqAny is disabled */
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

  /** Source version */
  object `3.0-migration`
  object `3.0`
  object `3.1-migration`
  object `3.1`
end language
