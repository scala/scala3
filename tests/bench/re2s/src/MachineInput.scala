// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/regexp.go

package java.util.regex

/**
 * MachineInput abstracts different representations of the input text
 * supplied to the Machine.  It provides one-character lookahead.
 */
abstract class MachineInput() {

  // Returns the rune at the specified index the units are
  // unspecified, but could be UTF-8 byte, UTF-16 char, or rune
  // indices.  Returns the width (in the same units) of the rune in
  // the lower 3 bits, and the rune (Unicode code point) in the high
  // bits.  Never negative, except for EOF which is represented as -1
  // << 3 | 0.
  def step(pos: Int): Int

  // can we look ahead without losing info?
  def canCheckPrefix(): Boolean

  // Returns the index relative to |pos| at which |re2.prefix| is found
  // in this input stream, or a negative value if not found.
  def index(re2: RE2, pos: Int): Int

  // Returns a bitmask of EMPTY_* flags.
  def context(pos: Int): Int

  // Returns the end position in the same units as step().
  def endPos(): Int
}

object MachineInput {

  final val EOF: Int = (-1 << 3) | 0

  def fromUTF8(b: Array[Byte], start: Int, end: Int): MachineInput =
    new UTF8Input(b, start, end)

  def fromUTF16(s: CharSequence, start: Int, end: Int): MachineInput =
    new UTF16Input(s, start, end)

  // An implementation of MachineInput for UTF-8 byte arrays.
  // |pos| and |width| are byte indices.
  private class UTF8Input(b: Array[Byte], start: Int, end: Int)
      extends MachineInput() {
    if (end > b.length) {
      throw new ArrayIndexOutOfBoundsException(
        "end is greater than length: " + end + " > " + b.length)
    }

    override def step(_i: Int): Int = {
      var i: Int = _i
      i += start
      if (i >= end) {
        return EOF
      }

      // UTF-8.  RFC 3629 in five lines:
      //
      // Unicode code points            UTF-8 encoding (binary)
      //         00-7F  (7 bits)   0tuvwxyz
      //     0080-07FF (11 bits)   110pqrst 10uvwxyz
      //     0800-FFFF (16 bits)   1110jklm 10npqrst 10uvwxyz
      // 010000-10FFFF (21 bits)   11110efg 10hijklm 10npqrst 10uvwxyz
      var x: Int = b(i) & 0xff // zero extend
      i += 1
      if ((x & 0x80) == 0) {
        return x << 3 | 1
      } else if ((x & 0xE0) == 0xC0) { // 110xxxxx
        x = x & 0x1F
        if (i >= end) {
          return EOF
        }
        x = x << 6 | b(i) & 0x3F
        i += 1
        return x << 3 | 2
      } else if ((x & 0xF0) == 0xE0) { // 1110xxxx
        x = x & 0x0F
        if (i + 1 >= end) {
          return EOF
        }
        x = x << 6 | b(i) & 0x3F
        i += 1
        x = x << 6 | b(i) & 0x3F
        i += 1
        return x << 3 | 3
      } else { // 11110xxx
        x = x & 0x07
        if (i + 2 >= end) {
          return EOF
        }
        x = x << 6 | b(i) & 0x3F
        i += 1
        x = x << 6 | b(i) & 0x3F
        i += 1
        x = x << 6 | b(i) & 0x3F
        i += 1
        return x << 3 | 4
      }
    }

    override def canCheckPrefix(): Boolean = true

    override def index(re2: RE2, _pos: Int): Int = {
      var pos: Int = _pos
      pos += start
      val i: Int = Utils.indexOf(b, re2.prefixUTF8, pos)
      if (i < 0) i else i - pos
    }

    override def context(_pos: Int): Int = {
      var pos: Int = _pos
      pos += this.start
      var r1: Int = -1
      if (pos > this.start && pos <= this.end) {
        var start: Int = pos - 1
        r1 = b(start)
        start -= 1
        if (r1 >= 0x80) { // decode UTF-8
          // Find start, up to 4 bytes earlier.
          var lim: Int = pos - 4
          if (lim < this.start) {
            lim = this.start
          }
          while (start >= lim && (b(start) & 0xC0) == 0x80) { // 10xxxxxx
            start -= 1
          }
          if (start < this.start) {
            start = this.start
          }
          r1 = step(start) >> 3
        }
      }
      val r2: Int = if (pos < this.end) step(pos) >> 3 else -1
      return Utils.emptyOpContext(r1, r2)
    }

    override def endPos(): Int = end
  }

  // |pos| and |width| are in Java "char" units.
  private class UTF16Input(str: CharSequence, start: Int, end: Int)
      extends MachineInput() {

    override def step(_pos: Int): Int = {
      var pos: Int = _pos
      pos += start
      if (pos < end) {
        val rune: Int    = Character.codePointAt(str, pos)
        val nextPos: Int = pos + Character.charCount(rune)
        val width: Int   = nextPos - pos
        return rune << 3 | width
      } else {
        return EOF
      }
    }

    override def canCheckPrefix(): Boolean = true

    override def index(re2: RE2, _pos: Int): Int = {
      var pos: Int = _pos
      pos += start
      val i: Int = indexOf(str, re2.prefix, pos)
      if (i < 0) i else i - pos
    }

    override def context(_pos: Int): Int = {
      var pos: Int = _pos
      pos += start
      val r1: Int =
        if (pos > start && pos <= end) Character.codePointBefore(str, pos)
        else -1
      val r2: Int = if (pos < end) Character.codePointAt(str, pos) else -1
      return Utils.emptyOpContext(r1, r2)
    }

    override def endPos(): Int = end

    private def indexOf(hayStack: CharSequence,
                        needle: String,
                        pos: Int): Int =
      hayStack match {
        case hayStack: String =>
          hayStack.indexOf(needle, pos)
        case hayStack: StringBuilder =>
          hayStack.indexOf(needle, pos)
        case _ =>
          indexOfFallback(hayStack, needle, pos)
      }

    // Modified version of {@link String#indexOf(String) that allows a CharSequence.
    private def indexOfFallback(hayStack: CharSequence,
                                needle: String,
                                _fromIndex: Int): Int = {
      var fromIndex: Int = _fromIndex

      if (fromIndex >= hayStack.length()) {
        return if (needle.isEmpty()) 0 else -1
      }
      if (fromIndex < 0) {
        fromIndex = 0
      }
      if (needle.isEmpty()) {
        return fromIndex
      }

      val first: Char = needle.charAt(0)
      val max: Int   = hayStack.length() - needle.length()
      var i: Int     = fromIndex

      while (i <= max) {
        /* Look for first character. */
        if (hayStack.charAt(i) != first) {
          while ({ i += 1; i } <= max && hayStack.charAt(i) != first) {}
        }

        /* Found first character, now look at the rest of v2 */
        if (i <= max) {
          var j: Int   = i + 1
          val end: Int = j + needle.length() - 1
          var k: Int   = 1
          while (j < end && hayStack.charAt(j) == needle.charAt(k)) {
            j += 1
            k += 1
          }
          if (j == end) {
            /* Found whole string. */
            return i
          }
        }

        i += 1
      }

      return -1
    }
  }
}
