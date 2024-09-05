// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package java.util.regex

/**
 * Various constants and helper utilities.
 */
object Utils {

  final val EMPTY_INTS: Array[Int] = new Array[Int](0)

  // Returns true iff |c| is an ASCII letter or decimal digit.
  def isalnum(c: Int): Boolean =
    '0' <= c && c <= '9' || 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'

  // If |c| is an ASCII hex digit, returns its value, otherwise -1.
  def unhex(c: Int): Int = {
    if ('0' <= c && c <= '9') {
      return c - '0'
    }
    if ('a' <= c && c <= 'f') {
      return c - 'a' + 10
    }
    if ('A' <= c && c <= 'F') {
      return c - 'A' + 10
    }
    return -1
  }

  private final val METACHARACTERS: String = "\\.+*?()|[]{}^$"

  // Appends a RE2 literal to |out| for rune |rune|,
  // with regexp metacharacters escaped.
  def escapeRune(out: java.lang.StringBuilder, rune: Int): Unit = {
    if (Unicode.isPrint(rune)) {
      if (METACHARACTERS.indexOf(rune.toChar) >= 0) {
        out.append('\\')
      }
      out.appendCodePoint(rune)
      return
    }

    rune match {
      case '"' =>
        out.append("\\\"")
      case '\\' =>
        out.append("\\\\")
      case '\t' =>
        out.append("\\t")
      case '\n' =>
        out.append("\\n")
      case '\r' =>
        out.append("\\r")
      case '\b' =>
        out.append("\\b")
      case '\f' =>
        out.append("\\f")
      case _ =>
        val s: String = Integer.toHexString(rune)
        if (rune < 0x100) {
          out.append("\\x")
          if (s.length() == 1) {
            out.append('0')
          }
          out.append(s)
        } else {
          out.append("\\x{").append(s).append('}')
        }
    }
  }

  // Returns the array of runes in the specified Java UTF-16 string.
  def stringToRunes(str: String): Array[Int] = {
    val charlen: Int = str.length()
    val runelen: Int = str.codePointCount(0, charlen)
    val runes: Array[Int]   = new Array[Int](runelen)
    var r: Int       = 0
    var c: Int       = 0
    while (c < charlen) {
      val rune: Int = str.codePointAt(c)
      runes(r) = rune
      r += 1
      c += Character.charCount(rune)
    }
    runes
  }

  // Returns the Java UTF-16 string containing the single rune |r|.
  def runeToString(r: Int): String = {
    val c: Char = r.toChar
    if (r == c) {
      String.valueOf(c)
    } else {
      new String(Character.toChars(c))
    }
  }

  // Returns a new copy of the specified subarray.
  def subarray_i(array: Array[Int], start: Int, end: Int): Array[Int] = {
    val r: Array[Int] = new Array[Int](end - start)
    var i: Int = start
    while (i < end) {
      r(i - start) = array(i)
      i += 1
    }
    r
  }

  // Returns a new copy of the specified subarray.
  def subarray_b(array: Array[Byte], start: Int, end: Int): Array[Byte] = {
    val r: Array[Byte] = new Array[Byte](end - start)
    var i: Int = start
    while (i < end) {
      r(i - start) = array(i)
      i += 1
    }
    r
  }

  // Returns the index of the first occurrence of array |target| within
  // array |source| after |fromIndex|, or -1 if not found.
  def indexOf(source: Array[Byte], target: Array[Byte], _fromIndex: Int): Int = {
    var fromIndex: Int = _fromIndex
    if (fromIndex >= source.length) {
      return (if (target.length == 0) source.length else -1)
    }
    if (fromIndex < 0) {
      fromIndex = 0
    }
    if (target.length == 0) {
      return fromIndex
    }

    val first: Byte = target(0)
    val max: Int   = source.length - target.length
    var i: Int     = fromIndex
    while (i <= max) {
      // Look for first byte.
      if (source(i) != first) {
        while ({ i += 1; i } <= max && source(i) != first) {}
      }

      // Found first byte, now look at the rest of v2.
      if (i <= max) {
        var j: Int   = i + 1
        val end: Int = j + target.length - 1
        var k: Int   = 1
        while (j < end && source(j) == target(k)) { j += 1; k += 1 }

        if (j == end) {
          return i // found whole array
        }
      }

      i += 1
    }
    return -1
  }

  // isWordRune reports whether r is consider a ``word character''
  // during the evaluation of the \b and \B zero-width assertions.
  // These assertions are ASCII-only: the word characters are [A-Za-z0-9_].
  def isWordRune(r: Int): Boolean =
    ('A' <= r && r <= 'Z' ||
      'a' <= r && r <= 'z' ||
      '0' <= r && r <= '9' ||
      r == '_')

  //// EMPTY_* flags

  final val EMPTY_BEGIN_LINE: Int       = 0x01
  final val EMPTY_END_LINE: Int         = 0x02
  final val EMPTY_BEGIN_TEXT: Int       = 0x04
  final val EMPTY_END_TEXT: Int         = 0x08
  final val EMPTY_WORD_BOUNDARY: Int    = 0x10
  final val EMPTY_NO_WORD_BOUNDARY: Int = 0x20
  final val EMPTY_ALL: Int              = -1 // (impossible)

  // emptyOpContext returns the zero-width assertions satisfied at the position
  // between the runes r1 and r2, a bitmask of EMPTY_* flags.
  // Passing r1 == -1 indicates that the position is at the beginning of the
  // text.
  // Passing r2 == -1 indicates that the position is at the end of the text.
  final def emptyOpContext(r1: Int, r2: Int): Int = {
    var op: Int = 0
    if (r1 < 0) {
      op |= EMPTY_BEGIN_TEXT | EMPTY_BEGIN_LINE
    }
    if (r1 == '\n') {
      op |= EMPTY_BEGIN_LINE
    }
    if (r2 < 0) {
      op |= EMPTY_END_TEXT | EMPTY_END_LINE
    }
    if (r2 == '\n') {
      op |= EMPTY_END_LINE
    }
    if (isWordRune(r1) != isWordRune(r2)) {
      op |= EMPTY_WORD_BOUNDARY
    } else {
      op |= EMPTY_NO_WORD_BOUNDARY
    }
    op
  }
}
