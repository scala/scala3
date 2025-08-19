package scala.annotation
import annotation.experimental

import language.experimental.captureChecking

/** Marks an annotated class as a capability.
 *  If the annotation is present and -Ycc is set, any (possibly aliased
 *  or refined) instance of the class type is implicitly augmented with
 *  the universal capture set. Example
 *
 *    @capability class CanThrow[T]
 *
 *  THere, the capture set of any instance of `CanThrow` is assumed to be
 *  `{*}`.
 */
@experimental
@deprecated("To make a class a capability, let it derive from the `Capability` trait instead")
final class capability extends StaticAnnotation
