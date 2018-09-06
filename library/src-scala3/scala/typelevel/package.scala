package scala

package object typelevel {

  erased def erasedValue[T]: T = ???

  case class Typed[T](val value: T) { type Type = T }

  rewrite def error(transparent msg: String, objs: Any*): Nothing = ???

  rewrite def constValueOpt[T]: Option[T] = ???

  rewrite def constValue[T]: T = ???

  type S[X <: Int] <: Int
}