class InlineError {
  Assertion.failAssert(this)
  val v = 2;
}

object Assertion:
  transparent inline def failAssert(inline message: => Any): Unit =
    scala.runtime.Scala3RunTime.assertFailed(message) // error