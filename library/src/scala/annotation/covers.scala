package scala.annotation

/** An annotation specifying that an extractor is irrefutable
 *  if the scrutinee is a subtype of `T`.
 *
 *  For example, the extractor `:+` covers non-empty list (`::[T]`):
 *
 *      object :+ {
 *        def unapply[T](l: List[T]): Option[(List[T], T)] @covers[::[T]] = ...
 *      }
 *
 *      def f(xs: List[Int]) =
 *        xs match
 *        case init :+ last => ()
 *        case Nil => ()
 *
 *  Therefore, the pattern match above is exhaustive.
 */
final class covers[T] extends StaticAnnotation
