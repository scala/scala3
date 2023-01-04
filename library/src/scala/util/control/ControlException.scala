package scala.util.control

/** A parent class for throwable objects intended for flow control.
 *
 *  Instances of ControlException don't record a stacktrace and are therefore
 *  much more efficient to throw than normal exceptions.
 *
 *  Unlike `ControlThrowable`, `ControlException` is a regular `RuntimeException`
 *  that is supposed to be handled like any other exception.
 *
 *  Instances of `ControlException` should not normally have a cause.
 *  Legacy subclasses may set a cause using `initCause`.
 */
abstract class ControlException(message: String | Null) extends RuntimeException(
  message, /*cause*/ null, /*enableSuppression=*/ false, /*writableStackTrace*/ false):

  def this() = this(message = null)

