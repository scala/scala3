class A(val x: Int)
class B(override val x: Int) extends A(x)

class C(x: Int) extends A(x)
case class D(override val x: Int) extends C(x)

// The following is extracted from akka:
trait LogEvent {
  def cause: Throwable
}

/**
 * For ERROR Logging
 */
case class Error(override val cause: Throwable) extends LogEvent
class Error2(override val cause: Throwable) extends Error(cause)
class Error3(override val cause: Throwable) extends Error2(cause)

