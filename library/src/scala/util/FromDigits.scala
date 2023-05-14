package scala.util
import scala.math.{BigInt}
import quoted._
import annotation.internal.sharable


/** A type class for types that admit numeric literals.
 */
trait FromDigits[T] {

  /** Convert `digits` string to value of type `T`
   *  `digits` can contain
   *  - sign `+` or `-`
   *  - sequence of digits between 0 and 9
   *
   * @throws MalformedNumber if digit string is not legal for the given type
   * @throws NumberTooLarge  if value of result does not fit into `T`'s range
   * @throws NumberTooSmall  in case of numeric underflow (e.g. a non-zero
   *                         floating point literal that produces a zero value)
   */
  def fromDigits(digits: String): T
}

object FromDigits {

  /** A subclass of `FromDigits` that also allows to convert whole number literals
   *  with a radix other than 10
   */
  trait WithRadix[T] extends FromDigits[T] {
    def fromDigits(digits: String): T = fromDigits(digits, 10)

    /** Convert digits string with given radix to number of type `T`.
     *  E.g. if radix is 16, digits `a..f` and `A..F` are also allowed.
     */
    def fromDigits(digits: String, radix: Int): T
  }

  /** A subclass of `FromDigits` that also allows to convert number
   *  literals containing a decimal point ".".
   */
  trait Decimal[T] extends FromDigits[T]

  /** A subclass of `FromDigits`that allows also to convert number
   *  literals containing a decimal point "." or an
   *  exponent `('e' | 'E')['+' | '-']digit digit*`.
   */
  trait Floating[T] extends Decimal[T]

  /** The base type for exceptions that can be thrown from
   *  `fromDigits` conversions
   */
  abstract class FromDigitsException(msg: String) extends NumberFormatException(msg)

  /** Thrown if value of result does not fit into result type's range */
  class NumberTooLarge(msg: String = "number too large") extends FromDigitsException(msg)

  /** Thrown in case of numeric underflow (e.g. a non-zero
   *  floating point literal that produces a zero value)
   */
  class NumberTooSmall(msg: String = "number too small") extends FromDigitsException(msg)

  /** Thrown if digit string is not legal for the given type */
  class MalformedNumber(msg: String = "malformed number literal") extends FromDigitsException(msg)

  /** Convert digits and radix to integer value (either int or Long)
   *  This is tricky because of the max negative value.
   *  Note: We cannot use java.lang.Integer.valueOf or java.lang.Long.valueOf
   *  since these do not handle unsigned hex numbers greater than the maximal value
   *  correctly.
   */
  private def integerFromDigits(digits: String, radix: Int, limit: Long): Long = {
    var value: Long = 0
    val divider = if (radix == 10) 1 else 2
    var i = 0
    var negated = false
    val len = digits.length
    if (0 < len && (digits(0) == '-' || digits(0) == '+')) {
      negated = digits(0) == '-'
      i += 1
    }
    if (i == len) throw MalformedNumber()
    while (i < len) {
      val ch = digits(i)
      val d =
        if (ch <= '9') ch - '0'
        else if ('a' <= ch && ch <= 'z') ch - 'a' + 10
        else if ('A' <= ch && ch <= 'Z') ch - 'A' + 10
        else -1
      if (d < 0 || radix <= d) throw MalformedNumber()
      if (value < 0 ||
          limit / (radix / divider) < value ||
          limit - (d / divider) < value * (radix / divider) &&
            !(negated && limit == value * radix - 1 + d)) throw NumberTooLarge()
      value = value * radix + d
      i += 1
    }
    if (negated) -value else value
  }

  /** Convert digit string to Int number
   *  @param digits            The string to convert
   *  @param radix             The radix
   *  @throws NumberTooLarge   if number does not fit within Int range
   *  @throws MalformedNumber  if digits is not a legal digit string.
   *                           Legal strings consist only of digits conforming to radix,
   *                           possibly preceded by a "-" sign.
   */
  def intFromDigits(digits: String, radix: Int = 10): Int =
    integerFromDigits(digits, radix, Int.MaxValue).toInt

  /** Convert digit string to Long number
   *  @param digits            The string to convert
   *  @param radix             The radix
   *  @throws NumberTooLarge   if the resulting number does not fit within Long range
   *  @throws MalformedNumber  if digits is not a legal digit string.
   *                           Legal strings consist only of digits conforming to radix,
   *                           possibly preceded by a "-" sign.
   */
  def longFromDigits(digits: String, radix: Int = 10): Long =
    integerFromDigits(digits, radix, Long.MaxValue)

  @sharable private val zeroFloat = raw"-?[0.]+(?:[eE][+-]?[0-9]+)?[fFdD]?".r

  /** Convert digit string to Float number
   *  @param digits            The string to convert
   *  @throws NumberTooLarge   if the resulting number is infinite
   *  @throws NumberTooSmall   if the resulting number is 0.0f, yet the digits
   *                           string contains non-zero digits before the exponent.
   *  @throws MalformedNumber  if digits is not a legal digit string for floating point numbers.
   */
  def floatFromDigits(digits: String): Float = {
    val x: Float =
      try java.lang.Float.parseFloat(digits)
      catch {
        case ex: NumberFormatException => throw MalformedNumber()
      }
    if (x.isInfinite) throw NumberTooLarge()
    if (x == 0.0f && !zeroFloat.pattern.matcher(digits).nn.matches) throw NumberTooSmall()
    x
  }

  /** Convert digit string to Double number
   *  @param digits            The string to convert
   *  @throws NumberTooLarge   if the resulting number is infinite
   *  @throws NumberTooSmall   if the resulting number is 0.0d, yet the digits
   *                           string contains non-zero digits before the exponent.
   *  @throws MalformedNumber  if digits is not a legal digit string for floating point numbers..
   */
  def doubleFromDigits(digits: String): Double = {
    val x: Double =
      try java.lang.Double.parseDouble(digits)
      catch {
        case ex: NumberFormatException => throw MalformedNumber()
      }
    if (x.isInfinite) throw NumberTooLarge()
    if (x == 0.0d && !zeroFloat.pattern.matcher(digits).nn.matches) throw NumberTooSmall()
    x
  }

  given BigIntFromDigits: WithRadix[BigInt] with {
    def fromDigits(digits: String, radix: Int): BigInt = BigInt(digits, radix)
  }

  given BigDecimalFromDigits: Floating[BigDecimal] with {
    def fromDigits(digits: String): BigDecimal = BigDecimal(digits)
  }
}
