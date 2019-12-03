package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to indicate a pair of type bounds that comes with a type.
 *  Used to indicate optional bounds of an opaque type
 */
class WithBounds[Lo <: AnyKind, Hi <: AnyKind] extends Annotation
