package scala

package object typelevel {

  erased def erasedValue[T]: T = ???

  case class Typed[T](val value: T) { type Type = T }

  inline def error(inline msg: String, objs: Any*): Nothing = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  type S[X <: Int] <: Int
}