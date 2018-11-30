package scala.typelevel

/** Every generic derivation starts with a typeclass instance of this type.
  *  It informs that type `T` has shape `S` and also implements runtime reflection on `T`.
  */
abstract class Shaped[T, S <: Shape] extends Reflected[T]
