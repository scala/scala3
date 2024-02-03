package scala.annotation

/** An annotation that indicates capture of a set of references under -Ycc.
 *
 *      T @retains(x, y, z)
 *
 *  is the internal representation used for the capturing type
 *
 *      {x, y, z}  T
 *
 *  The annotation can also be written explicitly if one wants to avoid the
 *  non-standard capturing type syntax.
 */
@experimental
class retains(xs: Any*) extends annotation.StaticAnnotation

/** Equivalent in meaning to `@retains(cap)`, but consumes less bytecode. 
 */
@experimental
class retainsCap() extends annotation.StaticAnnotation
  // This special case is needed to be able to load standard library modules without
  // cyclic reference errors. Specifically, load sequences involving IterableOnce.

