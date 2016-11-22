package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation to indicate a child class or object of the annotated class.
 *  E.g. if we have
 *
 *    sealed class A
 *    case class B() extends A
 *    case class C() extends A
 *
 *  Then the class symbol `A` would carry the annotations
 *  `@Child[Bref] @Child[Cref]` where `Bref`, `Cref` are TypeRefs
 *  referring to the class symbols of `B` and `C`
 */
class Child[T] extends Annotation
