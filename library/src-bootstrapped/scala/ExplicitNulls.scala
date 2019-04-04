package scala

/** This module defines extension methods for working with explicit nulls. */
delegate ExplicitNullsOps {

  /** Strips away the nullability from a value.
   *  e.g.
   *    val s1: String|Null = "hello"
   *    val s: String = s1.nn
   *
   *  Note that `.nn` performs a checked cast, so if invoked on a null value it'll throw an NPE.
   */
  def (x: T|Null) nn[T]: T =
    if (x == null) throw new NullPointerException("tried to cast away nullability, but value is null")
    else x.asInstanceOf[T]

  /** Reference equality where the receiver is a nullable union.
   *  Note that if the receiver `r` is a reference type (e.g. `String`), then `r.eq` will invoke the
   *  `eq` method in `AnyRef`.
   */
  def (x: AnyRef|Null) eq(y: AnyRef|Null): Boolean =
    (x == null && y == null) || (x != null && x.eq(y))

  /** Reference disequality where the receiver is a nullable union.
   *  Note that if the receiver `r` is a reference type (e.g. `String`), then `r.ne` will invoke the
   *  `ne` method in `AnyRef`.
   */
  def (x: AnyRef|Null) ne(y: AnyRef|Null): Boolean =
    (x == null && y != null) || (x != null && x.ne(y))
}