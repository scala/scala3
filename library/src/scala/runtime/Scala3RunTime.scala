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
  @deprecated("use Predef.nn instead", "3.2")
  def nn[T](x: T | Null): x.type & T =
    val isNull = x == null
    if isNull then nnFail()
    else x.asInstanceOf[x.type & T]

  /** Called by the inline extension def `nn`.
   *
   *  Extracted to minimize the bytecode size at call site.
   */
  def nnFail(): Nothing =
    throw new NullPointerException("tried to cast away nullability, but value is null")
end Scala3RunTime
