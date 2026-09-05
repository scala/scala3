package scala.runtime

import scala.language.`2.13`

import scala.math.ScalaNumber

/* The declaration of the class is only to make the JVM back-end happy when
 * compiling the scalalib.
 */
/** Never instantiated: this empty class exists only so that the JVM back-end accepts this file
 *  when compiling the Scala.js standard library. All operations live in the companion object.
 */
final class BoxesRunTime

object BoxesRunTime {
  /** Boxes a primitive `Boolean` into a `java.lang.Boolean`.
   *
   *  On Scala.js a `Boolean` is a primitive JavaScript boolean, which is its
   *  own boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param b the primitive value to box
   *  @return `b` as a `java.lang.Boolean`
   */
  def boxToBoolean(b: Boolean): java.lang.Boolean =
    b.asInstanceOf[java.lang.Boolean]

  /** Boxes a primitive `Char` into a `java.lang.Character`.
   *
   *  On Scala.js `Char` is the one primitive type with a distinct box class,
   *  so unlike the other `boxTo` methods this cast creates an actual
   *  `Character` wrapper around the character value.
   *
   *  @param c the primitive value to box
   *  @return `c` boxed as a `java.lang.Character`
   */
  def boxToCharacter(c: Char): java.lang.Character =
    c.asInstanceOf[java.lang.Character]

  /** Boxes a primitive `Byte` by casting it to a boxed representation.
   *
   *  On Scala.js a `Byte` is a primitive JavaScript number, which is its own
   *  boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param b the primitive value to box
   *  @return `b` in boxed form
   */
  def boxToByte(b: Byte): java.lang.Boolean =
    b.asInstanceOf[java.lang.Boolean]

  /** Boxes a primitive `Short` into a `java.lang.Short`.
   *
   *  On Scala.js a `Short` is a primitive JavaScript number, which is its
   *  own boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param s the primitive value to box
   *  @return `s` as a `java.lang.Short`
   */
  def boxToShort(s: Short): java.lang.Short =
    s.asInstanceOf[java.lang.Short]

  /** Boxes a primitive `Int` into a `java.lang.Integer`.
   *
   *  On Scala.js an `Int` is a primitive JavaScript number, which is its own
   *  boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param i the primitive value to box
   *  @return `i` as a `java.lang.Integer`
   */
  def boxToInteger(i: Int): java.lang.Integer =
    i.asInstanceOf[java.lang.Integer]

  /** Boxes a primitive `Long` into a `java.lang.Long`.
   *
   *  On Scala.js a `Long` is already represented by a heap object (a
   *  `RuntimeLong`), which is its own boxed representation, so the cast
   *  reinterprets the value without allocating a wrapper.
   *
   *  @param l the primitive value to box
   *  @return `l` as a `java.lang.Long`
   */
  def boxToLong(l: Long): java.lang.Long =
    l.asInstanceOf[java.lang.Long]

  /** Boxes a primitive `Float` into a `java.lang.Float`.
   *
   *  On Scala.js a `Float` is a primitive JavaScript number, which is its
   *  own boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param f the primitive value to box
   *  @return `f` as a `java.lang.Float`
   */
  def boxToFloat(f: Float): java.lang.Float =
    f.asInstanceOf[java.lang.Float]

  /** Boxes a primitive `Double` into a `java.lang.Double`.
   *
   *  On Scala.js a `Double` is a primitive JavaScript number, which is its
   *  own boxed representation, so the cast reinterprets the value without
   *  allocating a wrapper.
   *
   *  @param d the primitive value to box
   *  @return `d` as a `java.lang.Double`
   */
  def boxToDouble(d: Double): java.lang.Double =
    d.asInstanceOf[java.lang.Double]

  /** Unboxes a boxed `Boolean` into a primitive `Boolean`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `false`, matching the JVM runtime.
   *
   *  @param b the boxed value to unbox
   *  @return the primitive value of `b`, or `false` if `b` is `null`
   *  @throws ClassCastException if `b` is neither `null` nor a boxed
   *          `Boolean` (when compliant `asInstanceOf`s are enabled, the
   *          default)
   */
  def unboxToBoolean(b: Any): Boolean = b.asInstanceOf[Boolean]

  /** Unboxes a boxed `Character` into a primitive `Char`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to the null character (the `Char` with numeric value `0`), matching
   *  the JVM runtime.
   *
   *  @param c the boxed value to unbox
   *  @return the primitive value of `c`, or the null character if `c` is
   *          `null`
   *  @throws ClassCastException if `c` is neither `null` nor a boxed `Char`
   *          (when compliant `asInstanceOf`s are enabled, the default)
   */
  def unboxToChar(c: Any): Char = c.asInstanceOf[Char]

  /** Unboxes a boxed `Byte` into a primitive `Byte`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0`, and because boxed numbers are primitive JavaScript numbers, any
   *  boxed numeric value that is a whole number in the `Byte` range passes
   *  the cast (unlike on the JVM, where only a `java.lang.Byte` would).
   *
   *  @param b the boxed value to unbox
   *  @return the primitive value of `b`, or `0` if `b` is `null`
   *  @throws ClassCastException if `b` is neither `null` nor a number in the
   *          `Byte` range (when compliant `asInstanceOf`s are enabled, the
   *          default)
   */
  def unboxToByte(b: Any): Byte = b.asInstanceOf[Byte]

  /** Unboxes a boxed `Short` into a primitive `Short`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0`, and because boxed numbers are primitive JavaScript numbers, any
   *  boxed numeric value that is a whole number in the `Short` range passes
   *  the cast (unlike on the JVM, where only a `java.lang.Short` would).
   *
   *  @param s the boxed value to unbox
   *  @return the primitive value of `s`, or `0` if `s` is `null`
   *  @throws ClassCastException if `s` is neither `null` nor a number in the
   *          `Short` range (when compliant `asInstanceOf`s are enabled, the
   *          default)
   */
  def unboxToShort(s: Any): Short = s.asInstanceOf[Short]

  /** Unboxes a boxed `Int` into a primitive `Int`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0`, and because boxed numbers are primitive JavaScript numbers, any
   *  boxed numeric value that is a whole number in the `Int` range passes
   *  the cast (unlike on the JVM, where only a `java.lang.Integer` would).
   *
   *  @param i the boxed value to unbox
   *  @return the primitive value of `i`, or `0` if `i` is `null`
   *  @throws ClassCastException if `i` is neither `null` nor a number in the
   *          `Int` range (when compliant `asInstanceOf`s are enabled, the
   *          default)
   */
  def unboxToInt(i: Any): Int = i.asInstanceOf[Int]

  /** Unboxes a boxed `Long` into a primitive `Long`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0L`. Unlike the other numeric types, a `Long` is represented by a
   *  `RuntimeLong` instance rather than a JavaScript number, so only an
   *  actual boxed `Long` passes the cast.
   *
   *  @param l the boxed value to unbox
   *  @return the primitive value of `l`, or `0L` if `l` is `null`
   *  @throws ClassCastException if `l` is neither `null` nor a boxed `Long`
   *          (when compliant `asInstanceOf`s are enabled, the default)
   */
  def unboxToLong(l: Any): Long = l.asInstanceOf[Long]

  /** Unboxes a boxed `Float` into a primitive `Float`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0.0f`, and because boxed numbers are primitive JavaScript numbers,
   *  any boxed numeric value exactly representable as a `Float` passes the
   *  cast (unlike on the JVM, where only a `java.lang.Float` would).
   *
   *  @param f the boxed value to unbox
   *  @return the primitive value of `f`, or `0.0f` if `f` is `null`
   *  @throws ClassCastException if `f` is neither `null` nor a number
   *          exactly representable as a `Float` (when compliant
   *          `asInstanceOf`s are enabled, the default)
   */
  def unboxToFloat(f: Any): Float = f.asInstanceOf[Float]

  /** Unboxes a boxed `Double` into a primitive `Double`.
   *
   *  Implemented as a cast, so Scala.js cast semantics apply: `null` unboxes
   *  to `0.0`, and because every boxed number other than `Long` is a
   *  primitive JavaScript number, any such boxed numeric value passes the
   *  cast (unlike on the JVM, where only a `java.lang.Double` would).
   *
   *  @param d the boxed value to unbox
   *  @return the primitive value of `d`, or `0.0` if `d` is `null`
   *  @throws ClassCastException if `d` is neither `null` nor a JavaScript
   *          number (when compliant `asInstanceOf`s are enabled, the
   *          default)
   */
  def unboxToDouble(d: Any): Double = d.asInstanceOf[Double]

  /** Implements the universal equality test `x == y` for boxed values.
   *
   *  First compares `x` and `y` with JavaScript's strict equality operator
   *  (`===`). Because boxed numbers are primitive JavaScript numbers on
   *  Scala.js, this fast path already equates numerically equal values of
   *  different boxed types (a boxed `Int` `1` equals a boxed `Double` `1.0`),
   *  as well as identical object references and two `null`s. Only when `===`
   *  yields `false` does it fall back to the full dispatch of [[equals2]],
   *  which handles `Long`s, characters, `ScalaNumber`s and user-defined
   *  `equals` methods. `NaN` fails the fast path and is also unequal to
   *  itself in [[equals2]], preserving `Double.NaN != Double.NaN`.
   *
   *  @param x the left operand, may be `null`
   *  @param y the right operand, may be `null`
   *  @return `true` if `x` and `y` are equal by universal equality
   */
  def equals(x: Object, y: Object): Boolean =
    if (scala.scalajs.js.special.strictEquals(x, y)) true
    else equals2(x, y)

  /** The slow path of [[equals]]: universal equality dispatched on the type of `x`.
   *
   *  Routes boxed numbers to [[equalsNumObject]] and boxed characters to
   *  [[equalsCharObject]], so that numeric values compare equal across boxed
   *  types; a `null` `x` is equal only to a `null` `y`; any other `x` decides
   *  with its own `equals` method. On Scala.js the `java.lang.Number` case
   *  matches every boxed primitive number as well as `ScalaNumber`s such as
   *  `BigInt` and `BigDecimal`.
   *
   *  @param x the left operand, may be `null`
   *  @param y the right operand, may be `null`
   *  @return `true` if `x` and `y` are equal by universal equality
   */
  @inline // only called by equals(), not by codegen
  def equals2(x: Object, y: Object): Boolean = {
    x match {
      case xn: java.lang.Number    => equalsNumObject(xn, y)
      case xc: java.lang.Character => equalsCharObject(xc, y)
      case null                    => y eq null
      case _                       => x.equals(y)
    }
  }

  /** Compares a boxed number with an arbitrary object for universal equality.
   *
   *  If `y` is also a number, delegates to [[equalsNumNum]]; if `y` is a
   *  boxed `Character`, compares `xn` numerically against the character's
   *  integer value. Otherwise a `null` `xn` is equal only to a `null` `y`,
   *  and any other `xn` decides with its own `equals` method, which lets a
   *  `ScalaNumber` equate itself with objects of other types.
   *
   *  @param xn the boxed number, may be `null`
   *  @param y the right operand, may be `null`
   *  @return `true` if `xn` and `y` are equal by universal equality
   */
  def equalsNumObject(xn: java.lang.Number, y: Object): Boolean = {
    y match {
      case yn: java.lang.Number    => equalsNumNum(xn, yn)
      case yc: java.lang.Character => equalsNumChar(xn, yc)
      case _ =>
        if (xn eq null)
          y eq null
        else
          xn.equals(y)
    }
  }

  /** Compares two boxed numbers for universal equality, equating equal values across boxed types.
   *
   *  On Scala.js every boxed `Byte`, `Short`, `Integer`, `Float` and `Double`
   *  is a primitive JavaScript number and matches the `Double` cases, so the
   *  per-type dispatch of the JVM version collapses to two representations:
   *  JavaScript numbers and `Long`s. Within and across those two, values are
   *  compared with primitive `==`, converting a `Long` operand to `Double`
   *  when the other operand is a `Double`. When exactly one operand is a
   *  [[scala.math.ScalaNumber]] (such as `BigInt` or `BigDecimal`), that
   *  operand's `equals` method decides; a primitively-typed operand compared
   *  with any other kind of `Number` yields `false`, since its `equals`
   *  could not accept such an argument. If `xn` is itself neither a
   *  JavaScript number nor a `Long`, its own `equals` method decides. A
   *  `null` `xn` is equal only to a `null` `yn`.
   *
   *  @param xn the left boxed number, may be `null`
   *  @param yn the right boxed number, may be `null`
   *  @return `true` if `xn` and `yn` represent equal numeric values
   */
  def equalsNumNum(xn: java.lang.Number, yn: java.lang.Number): Boolean = {
    (xn: Any) match {
      case xn: Double =>
        (yn: Any) match {
          case yn: Double      => xn == yn
          case yn: Long        => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case xn: Long =>
        (yn: Any) match {
          case yn: Long        => xn == yn
          case yn: Double      => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case null => yn eq null
      case _    => xn.equals(yn)
    }
  }

  /** Compares a boxed `Character` with an arbitrary object for universal equality.
   *
   *  Two characters are equal if and only if their `charValue`s are equal. A
   *  character and a boxed number are compared numerically, using the
   *  character's integer value. Against any other `y`, a `null` `xc` is
   *  equal only to a `null` `y`, and a non-`null` `xc` is never equal, since
   *  its `equals` method could only accept another `Character`.
   *
   *  @param xc the boxed character, may be `null`
   *  @param y the right operand, may be `null`
   *  @return `true` if `xc` and `y` are equal by universal equality
   */
  def equalsCharObject(xc: java.lang.Character, y: Object): Boolean = {
    y match {
      case yc: java.lang.Character => xc.charValue() == yc.charValue()
      case yn: java.lang.Number    => equalsNumChar(yn, xc)
      case _ =>
        if (xc eq null)
          y eq null
        else
          false // xc.equals(y) must be false here, because y is not a Char
    }
  }

  @inline
  private def equalsNumChar(xn: java.lang.Number, yc: java.lang.Character): Boolean = {
    (xn: Any) match {
      case xn: Double => xn == yc.charValue()
      case xn: Long   => xn == yc.charValue()
      case _ =>
        if (xn eq null) yc eq null
        else xn.equals(yc)
    }
  }

  /** Returns the hash code of a boxed `Long`, consistent with universal equality.
   *
   *  Delegates to `Statics.longHash`: a value in the `Int` range hashes to
   *  that `Int` itself, and only values outside it fall back to
   *  `java.lang.Long.hashCode`. Because `==` equates numeric values across
   *  boxed types (`5L == 5`), equal values must hash alike, so `5L` must
   *  hash like the boxed `Int` `5`.
   *
   *  @param n the boxed `Long` to hash
   *  @return the hash code of `n`'s value
   */
  @inline
  def hashFromLong(n: java.lang.Long): Int =
    Statics.longHash(n.asInstanceOf[Long])

  /** Returns the hash code of a boxed `Double`, consistent with universal equality.
   *
   *  Delegates to `Statics.doubleHash`, which hashes a value equal to an
   *  `Int` as that `Int`, a value equal to some `Long` like that `Long`, a
   *  value exactly representable as a `Float` via
   *  `java.lang.Float.hashCode`, and any other value via
   *  `java.lang.Double.hashCode`. Because `==` equates numeric values across
   *  boxed types (`5.0 == 5L` and `5.0 == 5`), equal values must hash alike.
   *
   *  @param n the boxed `Double` to hash
   *  @return the hash code of `n`'s value
   */
  @inline
  def hashFromDouble(n: java.lang.Double): Int =
    Statics.doubleHash(n.asInstanceOf[Double])

  /** Returns the hash code of a boxed `Float`, consistent with universal equality.
   *
   *  Delegates to `Statics.floatHash`, which hashes a value equal to an
   *  `Int` as that `Int`, a value equal to some `Long` like that `Long`, and
   *  any other value via `java.lang.Float.hashCode`. Because `==` equates
   *  numeric values across boxed types (`5.0f == 5L` and `5.0f == 5`), equal
   *  values must hash alike.
   *
   *  @param n the boxed `Float` to hash
   *  @return the hash code of `n`'s value
   */
  @inline
  def hashFromFloat(n: java.lang.Float): Int =
    Statics.floatHash(n.asInstanceOf[Float])

  /** Returns the hash code of a boxed number, consistent with universal equality.
   *
   *  On Scala.js every boxed `Byte`, `Short`, `Integer`, `Float` and `Double`
   *  is a primitive JavaScript number and matches the `Double` case, so the
   *  dispatch collapses to `Statics.doubleHash` for JavaScript numbers,
   *  `Statics.longHash` for `Long`s, and the value's own `hashCode` for any
   *  other `Number` (such as `BigInt` or `BigDecimal`). The `Statics` hashes
   *  give numerically equal values of different boxed types the same hash,
   *  as required for values that `==` equates: `5`, `5L` and `5.0` all hash
   *  to `5`.
   *
   *  @param n the boxed number to hash
   *  @return the hash code of `n`'s value
   */
  @inline // called only by ScalaRunTime.hash()
  def hashFromNumber(n: java.lang.Number): Int = {
    (n: Any) match {
      case n: Double => Statics.doubleHash(n)
      case n: Long   => Statics.longHash(n)
      case n         => n.hashCode()
    }
  }

  /** Returns the hash code of any value, consistent with universal equality.
   *
   *  Delegates to `Statics.anyHash`: `null` hashes to `0`, boxed numbers
   *  hash by their numeric value so that values `==` equates hash alike, and
   *  every other value uses its own `hashCode`.
   *
   *  @param a the value to hash, may be `null`
   *  @return the hash code of `a`, or `0` if `a` is `null`
   */
  @inline
  def hashFromObject(a: Object): Int =
    Statics.anyHash(a)
}
