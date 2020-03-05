package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to record a Scala2 pickled alias.
 *  @param aliased  A TermRef pointing to the aliased field.
 *  TODO: Drop once the new param alias scheme is in the bootstrap compiler
 */
class Alias(aliased: Any) extends Annotation {

}
