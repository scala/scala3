package scala.typeclass

/** If a type class C extends this trait, the compiler is allowed
 *  to assume that for every type instance of `C`, all implicit
 *  instances of this type instance behave the same way.
 *
 *  Conversely, an implicit resolution of a coherent type class will
 *  never give an ambiguity error. If there are several candidates
 *  and none is more specific than the others, an arbitrary candidate
 *  is chosen.
 */
trait Coherent
