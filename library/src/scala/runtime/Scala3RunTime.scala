package scala.runtime

import language.experimental.captureChecking

object Scala3RunTime:

  // Called by inline def assert's. Extracted to minimize the bytecode size at call site.

  /** Throws the `AssertionError` for a failed `assert` with an explanation.
   *
   *  @param message the explanation of the failed assertion; converted to a
   *                 string and appended to the fixed prefix
   *  @throws java.lang.AssertionError always, with the message
   *          `"assertion failed: "` followed by `message`
   */
  def assertFailed(message: Any): Nothing =
    throw new java.lang.AssertionError("assertion failed: " + message)

  /** Throws the `AssertionError` for a failed `assert` without an explanation.
   *
   *  @throws java.lang.AssertionError always, with the fixed message
   *          `"assertion failed"`
   */
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
   *
   *  @throws NullPointerException always, indicating that a null value was encountered where non-null was expected
   */
  def nnFail(): Nothing =
    throw new NullPointerException("tried to cast away nullability, but value is null")
end Scala3RunTime
