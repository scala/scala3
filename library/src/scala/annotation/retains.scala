package scala.annotation

import language.experimental.captureChecking

/** An annotation that indicates capture of a set of references under capture checking.
 *
 *      T @retains[x.type | y.type | z.type]
 *
 *  is the internal representation used for the capturing type
 *
 *      T ^ {x, y, z}
 *
 *  The annotation can also be written explicitly if one wants to avoid the
 *  non-standard capturing type syntax.
 */
@experimental
class retains[Elems] extends annotation.StaticAnnotation

/** Equivalent in meaning to `@retains[any.type]`, but consumes less bytecode.
 */
@experimental
class retainsCap extends annotation.StaticAnnotation
  // This special case is needed to be able to load standard library modules without
  // cyclic reference errors. Specifically, load sequences involving IterableOnce.
