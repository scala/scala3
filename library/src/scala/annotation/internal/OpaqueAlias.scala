package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to record the right-hand side of an opaque type. Given
 *
 *    opaque type T = U
 *
 *  the info of `T` is `Nothing..Any`, but `T` carries the annotation `OpaqueAlias[U]`
 */
class OpaqueAlias[T] extends Annotation
