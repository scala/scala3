package dotty.annotation.internal

import scala.annotation.Annotation

/** An annotation to indicate a child class or object of the annotated class.
 *  E.g. if we have
 *
 *    sealed class A
 *    case class B() extends A
 *    case class C() extends A
 *
 *  Then `A` would carry the annotations `@Child[B] @Child[C]` where
 *  `B`, `C` are TypeRefs.
 */
class Child[T] extends Annotation
