package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to indicate a dynamic call is made during initialization of the object.
 *  E.g. if we have
 *
 *      abstract class A {
 *        def f: Int
 *        val x = f
 *      }
 *
 *  Then the class symbol `A` would carry the annotations `@Call[f]` where `f`
 *  is a TermRef referring to the term symbol of `f`.
 */
class Call[T] extends Annotation
