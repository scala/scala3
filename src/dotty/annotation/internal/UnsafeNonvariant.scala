package dotty.annotation.internal

import scala.annotation.Annotation

/** This annotation is used as a marker for unsafe
 *  instantiations in asSeenFrom. See TypeOps.asSeenfrom for an explanation.
 */
class UnsafeNonvariant extends Annotation
