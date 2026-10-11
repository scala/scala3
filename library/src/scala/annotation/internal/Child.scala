package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation to indicate a child class or object of the annotated class.
 *  E.g. if we have
 *
 *    sealed class A
 *    case class B() extends A
 *    case class C() extends A
 *
 *  Then the class symbol `A` would carry the annotations
 *  `@Child[Cref]`, @Child[Bref] where `Bref`, `Cref` are TypeRefs
 *  referring to the class symbols of `B` and `C`.
 *
 *  Child annotations always appear in reverse order of textual occurrence.
 *  I.e. in the example above, it is guaranteed that the child annotation for `C`
 *  appears before the one for `B`.
 *
 *  TODO: This should be `Child[T <: AnyKind]`
 *
 *  @tparam T a reference to the child class or object that extends the annotated sealed class, typically a `TypeRef` to the child's class symbol
 */
class Child[T] extends Annotation
