package scala.quoted

/** Throwing this error in the implementation of a macro
 *  will result in a compilation error with the given message.
 */
class QuoteError(message: String) extends Throwable(message)

object QuoteError {
  /** Throws a QuoteError with the given message */
  def apply(message: => String): Nothing = throw new QuoteError(message)
}
