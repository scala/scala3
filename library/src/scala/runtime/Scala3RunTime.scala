package scala.runtime

object Scala3RunTime:

  // Called by inline def assert's. Extracted to minimize the bytecode size at call site.

  def assertFailed(message: Any): Nothing =
    throw new java.lang.AssertionError("assertion failed: " + message)

  def assertFailed(): Nothing =
    throw new java.lang.AssertionError("assertion failed")

end Scala3RunTime
