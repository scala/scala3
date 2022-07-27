package scala
import annotation.experimental

/** An annotation that indicates capture
 */
@experimental class retains(xs: Any*) extends annotation.StaticAnnotation

