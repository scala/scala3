package scala.compat

/** A base class to be used for Scala enums that should be also exposed
 *  as Java enums.
 */
abstract class JEnum[E <: java.lang.Enum[E]](
  name: String = throw new UnsupportedOperationException,  // Compiler will pass actual values for these
  ordinal: Int = throw new UnsupportedOperationException)  // when JEnum is inherited in an enum
extends java.lang.Enum[E](name, ordinal)