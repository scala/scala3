package scala

package object compiletime {

  erased def erasedValue[T]: T = ???

  inline def error(inline msg: String): Nothing = ???

  /** Returns the string representations for code passed in the interpolated values
   *  ```scala
   *  inline def logged(p1: => Any) = {
   *    val c = code"code: $p1"
   *    val res = p1
   *    (c, p1)
   *  }
   *  logged(indentity("foo"))
   *  ```
   *  is equivalent to:
   *  ```scala
   *  ("code: indentity("foo")", indentity("foo"))
   *  ```
   */
  inline def (self: => StringContext) code (args: => Any*): String = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  type S[X <: Int] <: Int
}
