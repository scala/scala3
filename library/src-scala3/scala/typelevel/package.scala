package scala

package object typelevel {

  erased def erasedValue[T]: T = ???

  case class Typed[T](val value: T) { type Type = T }

  rewrite def error(transparent msg: String): Nothing = ???
}