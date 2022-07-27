package scala
import annotation.experimental

/** An annotation that indicates capture of an enclosing by-name type
 */
@experimental class retainsByName(xs: Any*) extends annotation.StaticAnnotation

