package scala

package object typelevel {

  final abstract class Type[-A, +B]
  type Exactly[T] = Type[T, T]
  type Subtype[T] = Type[_, T]
  type Supertype[T] = Type[T, _]

  erased def typeOf[T]: Type[T, T] = ???

  case class Typed[T](val value: T) { type Type = T }

  inline def error(inline msg: String, objs: Any*): Nothing = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  type S[X <: Int] <: Int
}
