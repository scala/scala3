package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** An annotation to record a Scala2 pickled alias.
 *  @param aliased  A TermRef pointing to the aliased field.
 */
class SourceFile(path: String) extends Annotation {

}
