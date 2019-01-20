package scala

package object compiletime {

  erased def erasedValue[T]: T = ???

  inline def error(inline msg: String, objs: Any*): Nothing = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  type S[X <: Int] <: Int
}