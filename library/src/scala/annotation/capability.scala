package scala.annotation

/** Marks an annotated class as a capabulity.
 *  If the annotation is present and -Ycc is set, any (possibly aliased
 *  or refined) instance of the class type is implicitly augmented with
 *  the universal capture set. Example
 *
 *    @capability class CanThrow[T]
 *
 *  THere, the capture set of any instance of `CanThrow` is assumed to be
 *  `{*}`.
 */
final class capability extends StaticAnnotation
