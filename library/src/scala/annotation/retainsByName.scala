package scala.annotation

import language.experimental.captureChecking

/** An annotation that indicates capture of an enclosing by-name type
 *
 *  @tparam Elems the capture set elements retained by the enclosing by-name type
 */
@experimental class retainsByName[Elems] extends annotation.StaticAnnotation

