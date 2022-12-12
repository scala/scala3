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
