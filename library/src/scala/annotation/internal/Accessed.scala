package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to record the method accessed by an inline accessor.
 *  @param aliased  A TermRef pointing to the accessed method
 */
class Accessed(aliased: Any) extends Annotation
