package scala.runtime

object Scala3RunTime:

  // Called by inline def assert's. Extracted to minimize the bytecode size at call site.

  def assertFailed(message: Any): Nothing =
    throw new java.lang.AssertionError("assertion failed: " + message)

  def assertFailed(): Nothing =
    throw new java.lang.AssertionError("assertion failed")

  /** Called by the inline extension def `nn`.
   *
   *  Extracted to minimize the bytecode size at call site.
   */
  def nn[T](x: T | Null): x.type & T =
    val isNull = x == null
    if (isNull) throw new NullPointerException("tried to cast away nullability, but value is null")
    else x.asInstanceOf[x.type & T]

  extension (inline x: AnyRef | Null)
    inline def eq(inline y: AnyRef | Null): Boolean =
      x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
    inline def ne(inline y: AnyRef | Null): Boolean =
      !(x eq y)

end Scala3RunTime
