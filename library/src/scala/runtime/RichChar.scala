/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime

import scala.language.`2.13`

/** A wrapper providing the additional methods available on `Char` values:
 *  comparisons, `to`/`until` range construction, and character classification
 *  and conversion methods that delegate to `java.lang.Character`.
 *
 *  @param self the wrapped `Char` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichChar(val self: Char) extends AnyVal with IntegralProxy[Char] {
  /** The `Integral` evidence for `Char`, [[scala.math.Numeric.CharIsIntegral]],
   *  which does arithmetic on the character's integer value.
   */
  protected def num: scala.math.Numeric.CharIsIntegral.type = scala.math.Numeric.CharIsIntegral
  /** The `Ordering` evidence for `Char`, [[scala.math.Ordering.Char]], which
   *  orders characters by their integer value.
   */
  protected def ord: scala.math.Ordering.Char.type = scala.math.Ordering.Char

  /** Returns this character's integer value (its UTF-16 code unit) as a `Double`. */
  override def doubleValue = self.toDouble
  /** Returns this character's integer value (its UTF-16 code unit) as a `Float`. */
  override def floatValue  = self.toFloat
  /** Returns this character's integer value (its UTF-16 code unit) as a `Long`. */
  override def longValue   = self.toLong
  /** Returns this character's integer value (its UTF-16 code unit) as an `Int`. */
  override def intValue    = self.toInt
  /** Returns the low 8 bits of this character's integer value as a `Byte`. */
  override def byteValue   = self.toByte
  /** Returns this character's 16-bit integer value reinterpreted as a signed `Short`. */
  override def shortValue  = self.toShort

  /** Always `true`, since the wrapped value is already a `Char`. */
  override def isValidChar   = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  /** Returns this character unchanged: a `Char` is unsigned, so it is its own
   *  absolute value.
   */
  override def abs: Char             = self
  /** Returns the larger of this character and `that`, comparing their integer
   *  values.
   *
   *  @param that the character to compare with
   */
  override def max(that: Char): Char = math.max(self.toInt, that.toInt).toChar
  /** Returns the smaller of this character and `that`, comparing their integer
   *  values.
   *
   *  @param that the character to compare with
   */
  override def min(that: Char): Char = math.min(self.toInt, that.toInt).toChar

  /** Returns the numeric value of this character as a digit in radix 36
   *  (`Character.MAX_RADIX`), or -1 if it is not a valid digit in that radix.
   *
   *  Digits `'0'` to `'9'` map to 0 through 9, and letters `'a'` to `'z'` (in
   *  either case) map to 10 through 35, so `'a'.asDigit` is 10. Digits and
   *  letters from other Unicode ranges are converted as well.
   */
  def asDigit: Int                      = Character.digit(self, Character.MAX_RADIX)

  /** Returns `true` if this character is an ISO control character: in the
   *  range U+0000 through U+001F or U+007F through U+009F.
   */
  def isControl: Boolean                = Character.isISOControl(self)
  /** Returns `true` if this character is a digit according to the Unicode
   *  standard (general category Nd), such as `'7'` or a digit from another
   *  script's decimal digit range.
   */
  def isDigit: Boolean                  = Character.isDigit(self)
  /** Returns `true` if this character is a Unicode letter (general category
   *  Lu, Ll, Lt, Lm, or Lo).
   */
  def isLetter: Boolean                 = Character.isLetter(self)
  /** Returns `true` if this character is a Unicode letter or digit. */
  def isLetterOrDigit: Boolean          = Character.isLetterOrDigit(self)
  /** Returns `true` if this character is white space according to Java: a
   *  Unicode space character that is not a non-breaking space, or one of the
   *  whitespace control characters such as tab, line feed, and carriage
   *  return (see `java.lang.Character.isWhitespace`).
   */
  def isWhitespace: Boolean             = Character.isWhitespace(self)
  /** Returns `true` if this character is a Unicode space character (general
   *  category Zs, Zl, or Zp). Unlike `isWhitespace`, this includes
   *  non-breaking spaces but excludes controls such as tab and line feed.
   */
  def isSpaceChar: Boolean              = Character.isSpaceChar(self)
  /** Returns `true` if this character is a UTF-16 high-surrogate code unit
   *  (U+D800 through U+DBFF), the first half of a surrogate pair.
   */
  def isHighSurrogate: Boolean          = Character.isHighSurrogate(self)
  /** Returns `true` if this character is a UTF-16 low-surrogate code unit
   *  (U+DC00 through U+DFFF), the second half of a surrogate pair.
   */
  def isLowSurrogate: Boolean           = Character.isLowSurrogate(self)
  /** Returns `true` if this character is a surrogate code unit, either high
   *  or low.
   */
  def isSurrogate: Boolean              = isHighSurrogate || isLowSurrogate
  /** Returns `true` if this character is permissible as the first character
   *  of a Unicode identifier.
   */
  def isUnicodeIdentifierStart: Boolean = Character.isUnicodeIdentifierStart(self)
  /** Returns `true` if this character is permissible after the first
   *  character of a Unicode identifier.
   */
  def isUnicodeIdentifierPart: Boolean  = Character.isUnicodeIdentifierPart(self)
  /** Returns `true` if this character is ignorable in a Java or Unicode
   *  identifier: a non-whitespace ISO control character or a formatting
   *  character (general category Cf).
   */
  def isIdentifierIgnorable: Boolean    = Character.isIdentifierIgnorable(self)
  /** Returns `true` if this character is mirrored according to the Unicode
   *  specification, that is, its glyph is mirrored horizontally when
   *  displayed in right-to-left text, as `'('` is.
   */
  def isMirrored: Boolean               = Character.isMirrored(self)

  /** Returns `true` if this character is a lowercase character. */
  def isLower: Boolean                  = Character.isLowerCase(self)
  /** Returns `true` if this character is an uppercase character. */
  def isUpper: Boolean                  = Character.isUpperCase(self)
  /** Returns `true` if this character is a titlecase letter (general
   *  category Lt), such as `'ǅ'`.
   */
  def isTitleCase: Boolean              = Character.isTitleCase(self)

  /** Returns the lowercase form of this character if it has one, or the
   *  character itself otherwise.
   *
   *  The mapping uses Unicode case information but is locale-insensitive and
   *  can only map one character to one character; for locale-sensitive text,
   *  use the `toLowerCase` method of `String` instead.
   */
  def toLower: Char                     = Character.toLowerCase(self)
  /** Returns the uppercase form of this character if it has one, or the
   *  character itself otherwise.
   *
   *  The mapping uses Unicode case information but is locale-insensitive and
   *  can only map one character to one character; for locale-sensitive text,
   *  use the `toUpperCase` method of `String` instead.
   */
  def toUpper: Char                     = Character.toUpperCase(self)
  /** Returns the titlecase form of this character if it has one; otherwise
   *  its uppercase form if it has one; otherwise the character itself.
   */
  def toTitleCase: Char                 = Character.toTitleCase(self)

  /** Returns an `Int` code for this character's Unicode general category,
   *  equal to one of the category constants of `java.lang.Character` such as
   *  `Character.UPPERCASE_LETTER`.
   */
  def getType: Int                      = Character.getType(self)
  /** Returns the `Int` value this character represents, such as 5 for `'5'`;
   *  letters map to 10 through 35 regardless of case, so both `'A'` and
   *  `'a'` yield 10. Returns -1 if the character has no numeric value, or -2
   *  if its numeric value is negative or not an integer (as for fraction
   *  characters).
   */
  def getNumericValue: Int              = Character.getNumericValue(self)
  /** Returns the Unicode directionality of this character as a `Byte`, equal
   *  to one of the `DIRECTIONALITY_` constants of `java.lang.Character`;
   *  `Character.DIRECTIONALITY_UNDEFINED` if the directionality is
   *  undefined.
   */
  def getDirectionality: Byte           = Character.getDirectionality(self)
  /** Returns the character obtained by swapping the two bytes of this
   *  character's 16-bit value.
   */
  def reverseBytes: Char                = Character.reverseBytes(self)

  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)
}
