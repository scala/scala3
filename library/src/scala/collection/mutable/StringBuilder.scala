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

package scala.collection.mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.collection.{IterableFactoryDefaults, IterableOnce}
import scala.collection.immutable.WrappedString

import scala.Predef.{ // unimport char-related implicit conversions to avoid triggering them accidentally
  genericArrayOps => _,
  charArrayOps => _,
  genericWrapArray => _,
  wrapCharArray => _,
  wrapString => _,
 //_
}

/** A builder of `String` which is also a mutable sequence of characters.
 *
 *  This class provides an API mostly compatible with `java.lang.StringBuilder`,
 *  except where there are conflicts with the Scala collections API, such as the `reverse` method:
 *  [[reverse]] produces a new `StringBuilder`, and [[reverseInPlace]] mutates this builder.
 *
 *  Mutating operations return either `this.type`, i.e., the current builder, or `Unit`.
 *
 *  Other methods extract data or information from the builder without mutating it.
 *
 *  The distinction is also reflected in naming conventions used by collections,
 *  such as `append`, which mutates, and `appended`, which does not, or `reverse`,
 *  which does not mutate, and `reverseInPlace`, which does.
 *
 *  The `String` result may be obtained using either `result()` or `toString`.
 *
 *  $multipleResults
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#stringbuilders)
 *  section on `StringBuilders` for more information.
 *
 *  @define Coll `mutable.IndexedSeq`
 *  @define coll string builder
 */
@SerialVersionUID(3L)
final class StringBuilder(val underlying: java.lang.StringBuilder) extends AbstractSeq[Char]
  with ReusableBuilder[Char, String]
  with IndexedSeq[Char]
  with IndexedSeqOps[Char, IndexedSeq, StringBuilder]
  with IterableFactoryDefaults[Char, IndexedSeq]
  with java.lang.CharSequence
  with Serializable {

  /** Constructs a string builder with no characters in it and an
   *  initial capacity of 16 characters.
   */
  def this() = this(new java.lang.StringBuilder)

  /** Constructs a string builder with no characters in it and an
   *  initial capacity specified by the `capacity` argument.
   *
   *  @param  capacity  the initial capacity.
   *  @throws java.lang.NegativeArraySizeException  if capacity < 0.
   */
  def this(capacity: Int) = this(new java.lang.StringBuilder(capacity))

  /** Constructs a string builder with initial characters
   *  equal to characters of `str`.
   *
   *  @param str the initial string content of this builder
   */
  def this(str: String) = this(new java.lang.StringBuilder(str))

  /** Constructs a string builder initialized with string value `initValue`
   *  and with additional character capacity `initCapacity`.
   *
   *  @param initCapacity additional character capacity beyond the length of `initValue`
   *  @param initValue the initial string content of this builder
   */
  def this(initCapacity: Int, initValue: String) =
    this(new java.lang.StringBuilder(initValue.length + initCapacity).append(initValue))

  // Methods required to make this an IndexedSeq:
  /** Returns the character at the specified index.
   *
   *  @param i the index of the character to return
   *  @return the character at index `i`
   *  @throws IndexOutOfBoundsException if `i` is out of bounds
   */
  def apply(i: Int): Char = underlying.charAt(i)

  /** Returns a new string builder containing the characters of `coll`.
   *
   *  @param coll the characters for the new string builder
   *  @return a new string builder with the characters of `coll` appended to it
   */
  override protected def fromSpecific(coll: scala.collection.IterableOnce[Char]^): StringBuilder =
    new StringBuilder() appendAll coll

  /** Returns a builder that produces a `StringBuilder`, backed by a new empty string builder. */
  override protected def newSpecificBuilder: Builder[Char, StringBuilder] =
    new GrowableBuilder(new StringBuilder())

  /** Returns a new empty string builder. */
  override def empty: StringBuilder = new StringBuilder()

  /** Returns the number of characters in this builder. */
  @inline def length: Int = underlying.length

  /** Setter alias for [[setLength]]: sets the length of the character sequence to
   *  `n`, truncating it or padding it with null characters (`'\u0000'`) as needed.
   *
   *  @throws IndexOutOfBoundsException if `n` is negative
   */
  def length_=(n: Int): Unit = underlying.setLength(n)

  /** Returns the number of characters in this builder, which is always known.
   *
   *  This override selects the `IndexedSeqOps` implementation, which returns
   *  `length`, over the `Growable` default, which returns -1.
   */
  override def knownSize: Int = super[IndexedSeqOps].knownSize

  /** Appends the given character to this builder.
   *
   *  @param x the character to append
   *  @return this string builder
   */
  def addOne(x: Char): this.type = { underlying.append(x); this }

  /** Removes all characters from this builder, setting its length to 0.
   *  The builder's capacity is unchanged, and it can be used again.
   */
  def clear(): Unit = underlying.setLength(0)

  /** Overloaded version of `addAll` that takes a string.
   *
   *  @param s the string to append to this builder
   *  @return this `StringBuilder`
   */
  def addAll(s: String): this.type = { underlying.append(s); this }

  /** Alias for `addAll`. */
  def ++= (s: String): this.type = addAll(s)

  /** Returns the contents of this builder as a `String`.
   *
   *  Unlike most builders, calling `result()` does not reset this builder:
   *  it may be called repeatedly, interleaved with further modifications.
   */
  def result() = underlying.toString

  /** Returns the contents of this builder as a `String`. Equivalent to [[result]]. */
  override def toString(): String = result()

  /** Returns an array containing the characters of this builder.
   *
   *  When `B` is `Char` itself, the characters are copied directly, as by
   *  [[toCharArray]]; for any other element type the generic implementation
   *  inherited from the collections library is used.
   *
   *  @tparam B the element type of the resulting array, a supertype of `Char`
   *  @param ct a class tag whose runtime class determines the component type
   *            of the resulting array
   */
  override def toArray[B >: Char](implicit ct: scala.reflect.ClassTag[B]) =
    ct.runtimeClass match {
      case java.lang.Character.TYPE => toCharArray.asInstanceOf[Array[B]]
      case _ => super.toArray
    }

  /** Returns the contents of this StringBuilder as an `Array[Char]`.
   *
   *  @return  An array with the characters from this builder.
   */
  def toCharArray: Array[Char] = {
    val len = underlying.length
    val arr = new Array[Char](len)
    underlying.getChars(0, len, arr, 0)
    arr
  }

  // append* methods delegate to the underlying java.lang.StringBuilder:

  /** Appends the given string to this builder.
   *
   *  @param xs the string to append
   *  @return this string builder
   */
  def appendAll(xs: String): this.type = {
    underlying.append(xs)
    this
  }

  /** Appends the string representation of the given argument,
   *  which is converted to a String with `String.valueOf`.
   *
   *  @param  x   an `Any` object.
   *  @return     this StringBuilder.
   */
  def append(x: Any): this.type = {
    underlying.append(String.valueOf(x))
    this
  }

  /** Appends the given String to this sequence.
   *
   *  @param  s   a String.
   *  @return     this StringBuilder.
   */
  def append(s: String): this.type = {
    underlying.append(s)
    this
  }

  /** Appends the given CharSequence to this sequence.
   *
   *  @param  cs   a CharSequence.
   *  @return     this StringBuilder.
   */
  def append(cs: java.lang.CharSequence): this.type = {
    underlying.append(cs match {
      // Both cases call into append(<CharSequence>), but java SB
      // looks up type at runtime and has fast path for SB.
      case s: StringBuilder => s.underlying
      case _                => cs
    })
    this
  }

  /** Appends the specified string builder to this sequence.
   *
   *  @param s
   *  @return
   */
  def append(s: StringBuilder): this.type = {
    underlying.append(s.underlying)
    this
  }

  /** Appends all the Chars in the given IterableOnce[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     this StringBuilder.
   */
  def appendAll(xs: IterableOnce[Char]^): this.type = {
    xs match {
      case x: WrappedString => underlying.append(x.unwrap)
      case x: ArraySeq.ofChar => underlying.append(x.array)
      case x: StringBuilder => underlying.append(x.underlying)
      case _ =>
        val ks = xs.knownSize
        if (ks != 0) {
          val b = underlying
          if (ks > 0) b.ensureCapacity(b.length + ks)
          val it = xs.iterator
          while (it.hasNext) { b.append(it.next()) }
        }
    }
    this
  }

  /** Appends all the Chars in the given Array[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     a reference to this object.
   */
  def appendAll(xs: Array[Char]): this.type = {
    underlying.append(xs)
    this
  }

  /** Appends a portion of the given Array[Char] to this sequence.
   *
   *  @param  xs      the Array containing Chars to be appended.
   *  @param  offset  the index of the first Char to append.
   *  @param  len     the numbers of Chars to append.
   *  @return         this StringBuilder.
   */
  def appendAll(xs: Array[Char], offset: Int, len: Int): this.type = {
    underlying.append(xs, offset, len)
    this
  }

  /** Appends the String representation of the given primitive type
   *  to this sequence.  The argument is converted to a String with
   *  String.valueOf.
   *
   *  @param   x  a primitive value
   *  @return     This StringBuilder.
   */
  def append(x: Boolean): this.type = { underlying.append(x) ; this }
  /** Appends the string representation of the given `Byte` value,
   *  as produced by `String.valueOf`, to this builder. The value is
   *  widened to an `Int` before conversion.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Byte): this.type = append(x.toInt)
  /** Appends the string representation of the given `Short` value,
   *  as produced by `String.valueOf`, to this builder. The value is
   *  widened to an `Int` before conversion.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Short): this.type = append(x.toInt)
  /** Appends the string representation of the given `Int` value,
   *  as produced by `String.valueOf`, to this builder.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Int): this.type = { underlying.append(x) ; this }
  /** Appends the string representation of the given `Long` value,
   *  as produced by `String.valueOf`, to this builder.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Long): this.type = { underlying.append (x) ; this }
  /** Appends the string representation of the given `Float` value,
   *  as produced by `String.valueOf`, to this builder.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Float): this.type = { underlying.append (x) ; this }
  /** Appends the string representation of the given `Double` value,
   *  as produced by `String.valueOf`, to this builder.
   *
   *  @param x the value to append
   *  @return this string builder
   */
  def append(x: Double): this.type = { underlying.append(x) ; this }
  /** Appends the given character to this builder.
   *
   *  @param x the character to append
   *  @return this string builder
   */
  def append(x: Char): this.type = { underlying.append(x) ; this }

  /** Removes a subsequence of Chars from this sequence, starting at the
   *  given start index (inclusive) and extending to the end index (exclusive)
   *  or to the end of the String, whichever comes first.
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @return        This StringBuilder.
   *  @throws StringIndexOutOfBoundsException   if start < 0 || start > end
   */
  def delete(start: Int, end: Int): this.type = {
    underlying.delete(start, end)
    this
  }

  /** Replaces a subsequence of Chars with the given String.  The semantics
   *  are as in delete, with the String argument then inserted at index 'start'.
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @param  str    The String to be inserted at the start index.
   *  @return        This StringBuilder.
   *  @throws StringIndexOutOfBoundsException if start < 0, start > length, or start > end
   */
  def replace(start: Int, end: Int, str: String): this.type = {
    underlying.replace(start, end, str)
    this
  }

  /** Inserts a subarray of the given Array[Char] at the given index
   *  of this sequence.
   *
   *  @param  index   index at which to insert the subarray.
   *  @param  str     the Array from which Chars will be taken.
   *  @param  offset  the index of the first Char to insert.
   *  @param  len     the number of Chars from 'str' to insert.
   *  @return         This StringBuilder.
   *
   *  @throws StringIndexOutOfBoundsException  if index < 0, index > length,
   *         offset < 0, len < 0, or (offset + len) > str.length.
   */
  def insertAll(index: Int, str: Array[Char], offset: Int, len: Int): this.type = {
    underlying.insert(index, str, offset, len)
    this
  }

  /** Inserts the String representation (via String.valueOf) of the given
   *  argument into this sequence at the given index.
   *
   *  @param  index   the index at which to insert.
   *  @param  x       a value.
   *  @return         this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insert(index: Int, x: Any): this.type = insert(index, String.valueOf(x))

  /** Inserts the String into this character sequence.
   *
   *  @param  index the index at which to insert.
   *  @param  x     a String.
   *  @return       this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insert(index: Int, x: String): this.type = {
    underlying.insert(index, x)
    this
  }

  /** Inserts the given `Seq[Char]` into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the `Seq[Char]`.
   *  @return       this `StringBuilder`.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: IterableOnce[Char]^): this.type =
    insertAll(index, (ArrayBuilder.make[Char] ++= xs).result())

  /** Inserts the given `Array[Char]` into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the `Array[Char]`.
   *  @return       this `StringBuilder`.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: Array[Char]): this.type = {
    underlying.insert(index, xs)
    this
  }

  /** Calls String.valueOf on the given primitive value, and inserts the
   *  String at the given index.
   *
   *  @param  index the offset position.
   *  @param  x     a primitive value.
   *  @return       this StringBuilder.
   */
  def insert(index: Int, x: Boolean): this.type = insert(index, String.valueOf(x))
  /** Inserts the string representation of the given `Byte` value,
   *  as produced by `String.valueOf`, at the given index. The value is
   *  widened to an `Int` before conversion.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Byte): this.type    = insert(index, x.toInt)
  /** Inserts the string representation of the given `Short` value,
   *  as produced by `String.valueOf`, at the given index. The value is
   *  widened to an `Int` before conversion.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Short): this.type   = insert(index, x.toInt)
  /** Inserts the string representation of the given `Int` value,
   *  as produced by `String.valueOf`, at the given index.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Int): this.type     = insert(index, String.valueOf(x))
  /** Inserts the string representation of the given `Long` value,
   *  as produced by `String.valueOf`, at the given index.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Long): this.type    = insert(index, String.valueOf(x))
  /** Inserts the string representation of the given `Float` value,
   *  as produced by `String.valueOf`, at the given index.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Float): this.type   = insert(index, String.valueOf(x))
  /** Inserts the string representation of the given `Double` value,
   *  as produced by `String.valueOf`, at the given index.
   *
   *  @param index the index at which to insert
   *  @param x the value to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Double): this.type  = insert(index, String.valueOf(x))
  /** Inserts the given character at the given index.
   *
   *  @param index the index at which to insert
   *  @param x the character to insert
   *  @return this string builder
   *  @throws StringIndexOutOfBoundsException if the index is out of bounds
   */
  def insert(index: Int, x: Char): this.type    = insert(index, String.valueOf(x))

  /** Sets the length of the character sequence.  If the current sequence
   *  is shorter than the given length, it is padded with nulls; if it is
   *  longer, it is truncated.
   *
   *  @param  len  the new length
   *  @throws IndexOutOfBoundsException if the argument is negative.
   */
  def setLength(len: Int): Unit = underlying.setLength(len)

  /** Replaces the character at index `idx` with `elem`.
   *
   *  Equivalent to [[setCharAt]]; enables assignment syntax `sb(idx) = elem`.
   *
   *  @param idx the index of the character to replace
   *  @param elem the replacement character
   *  @throws IndexOutOfBoundsException if `idx` is negative or not less than `length`
   */
  def update(idx: Int, elem: Char): Unit = underlying.setCharAt(idx, elem)


  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  @deprecated("Use reverseInPlace instead", "2.13.0")
  final def reverseContents(): this.type = reverseInPlace()

  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  def reverseInPlace(): this.type = {
    underlying.reverse()
    this
  }


  /** Returns the current capacity, which is the size of the underlying array.
   *  A new array will be allocated if the current capacity is exceeded.
   *
   *  @return  the capacity
   */
  def capacity: Int = underlying.capacity

  /** Ensures that the capacity is at least the given argument.
   *  If the argument is greater than the current capacity, new
   *  storage will be allocated with size equal to the given
   *  argument or to `(2 * capacity + 2)`, whichever is larger.
   *
   *  @param newCapacity    the minimum desired capacity.
   */
  def ensureCapacity(newCapacity: Int): Unit = { underlying.ensureCapacity(newCapacity) }

  /** Returns the Char at the specified index, counting from 0 as in Arrays.
   *
   *  @param  index   the index to look up
   *  @return         the Char at the given index.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def charAt(index: Int): Char = underlying.charAt(index)

  /** Removes the Char at the specified index.  The sequence is
   *  shortened by one.
   *
   *  @param  index  The index to remove.
   *  @return        This StringBuilder.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def deleteCharAt(index: Int): this.type = {
    underlying.deleteCharAt(index)
    this
  }

  /** Updates the sequence at the given index to hold the specified Char.
   *
   *  @param  index   the index to modify.
   *  @param  ch      the new Char.
   *  @return         this `StringBuilder`.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def setCharAt(index: Int, ch: Char): this.type = {
    underlying.setCharAt(index, ch)
    this
  }

  /** Returns a new String made up of a subsequence of this sequence,
   *  beginning at the given index and extending to the end of the sequence.
   *
   *  target.substring(start)  is equivalent to  target.drop(start)
   *
   *  @param  start  The starting index, inclusive.
   *  @return        The new String.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def substring(start: Int): String = underlying.substring(start, length)

  /** Returns a new String made up of a subsequence of this sequence,
   *  beginning at the start index (inclusive) and extending to the
   *  end index (exclusive).
   *
   *  target.substring(start, end)  is equivalent to  target.slice(start, end).mkString
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @return The new String.
   *  @throws StringIndexOutOfBoundsException If either index is out of bounds,
   *          or if start > end.
   */
  def substring(start: Int, end: Int): String = underlying.substring(start, end)

  /** For implementing CharSequence.
   *
   *  @param start the beginning index, inclusive
   *  @param end the ending index, exclusive
   *  @return the subsequence between `start` and `end` as a `CharSequence`
   */
  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    underlying.substring(start, end)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String): Int = underlying.indexOf(str)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String, fromIndex: Int): Int = underlying.indexOf(str, fromIndex)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String): Int = underlying.lastIndexOf(str)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String, fromIndex: Int): Int = underlying.lastIndexOf(str, fromIndex)

  /** Tests whether this builder is empty.
   *
   *  This method is required for JDK15+ compatibility
   *
   *  @return  `true` if this builder contains nothing, `false` otherwise.
   */
  override def isEmpty: Boolean = underlying.length() == 0
}

object StringBuilder {
  /** Returns a new empty string builder, with the default initial capacity of 16 characters. */
  @deprecated("Use `new StringBuilder()` instead of `StringBuilder.newBuilder`", "2.13.0")
  def newBuilder = new StringBuilder
}
