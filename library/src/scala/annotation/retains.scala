package scala.annotation

/** An annotation that indicates capture of a set of references under capture checking.
 *
 *      T @retains(x, y, z)
 *
 *  is the internal representation used for the capturing type
 *
 *      T ^ {x, y, z}
 *
 *  The annotation can also be written explicitly if one wants to avoid the
 *  non-standard capturing type syntax.
 */
@experimental
class retains(xs: (Any@retainsArg)*) extends annotation.StaticAnnotation

/** Equivalent in meaning to `@retains(cap)`, but consumes less bytecode.
 */
@experimental
class retainsCap() extends annotation.StaticAnnotation
  // This special case is needed to be able to load standard library modules without
  // cyclic reference errors. Specifically, load sequences involving IterableOnce.

/** Internal use, only for parameters of `retains` and `retainsByName`.
 */
@experimental
class retainsArg extends annotation.StaticAnnotation
  // This annotation prevents argument references to retains and retainsByName from being
  // augmented with explicit arguments. That's unsound in general, but necessary
  // since a captureRef could have an impure context function type, A ?=> B, but
  // we still need to have the unapplied captureRef in the annotation.
