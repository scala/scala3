package scala.quoted

/** Throwing this error in the implementation of a macro
 *  will result in a compilation error with the given message.
 */
class QuoteError(message: String, val from: Option[Expr[_]]) extends Throwable(message)

object QuoteError {
  /** Throws a QuoteError with the given message */
  def apply(message: => String): Nothing = throw new QuoteError(message, None)

  /** Throws a QuoteError with the given message at the position of the `from` expression */
  def apply(message: => String, from: Expr[_]): Nothing = throw new QuoteError(message, Some(from))
}
