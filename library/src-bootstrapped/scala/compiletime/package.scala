package scala

package object compiletime {

  erased def erasedValue[T]: T = ???

  inline def error(inline msg: String, objs: Any*): Nothing = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  type S[X <: Int] <: Int

  inline def findImplied[T] given (ev: (T | Null) = null): Option[ev.type] = inline ev match {
    case _: Null => None
    case _ => Some(ev)
  }
}