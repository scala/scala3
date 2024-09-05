// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/parse.go

package java.util.regex

import java.util.regex.CharClass._

/**
 * A "builder"-style helper class for manipulating character classes
 * represented as an array of pairs of runes [lo, hi], each denoting an
 * inclusive interval.
 *
 * All methods mutate the internal state and return {@code this}, allowing
 * operations to be chained.
 */
// inclusive ranges, pairs of [lo,hi].  r.length is even.
class CharClass(private var r: Array[Int]) {
  // prefix of |r| that is defined.  Even.
  private var len: Int = r.length

  // After a call to ensureCapacity(), |r.length| is at least |newLen|.
  private def ensureCapacity(_newLen: Int): Unit = {
    var newLen: Int = _newLen
    if (r.length < newLen) {
      // Expand by at least doubling, except when len == 0.
      if (newLen < len * 2) {
        newLen = len * 2
      }
      val r2: Array[Int] = new Array[Int](newLen)
      System.arraycopy(r, 0, r2, 0, len)
      r = r2
    }
  }

  // Returns the character class as an int array.  Subsequent CharClass
  // operations may mutate this array, so typically this is the last operation
  // performed on a given CharClass instance.
  def toArray(): Array[Int] = {
    if (this.len == r.length) {
      r
    } else {
      val r2: Array[Int] = new Array[Int](len)
      System.arraycopy(r, 0, r2, 0, len)
      r2
    }
  }

  // cleanClass() sorts the ranges (pairs of elements) of this CharClass,
  // merges them, and eliminates duplicates.
  def cleanClass(): CharClass = {
    if (len < 4) {
      return this
    }

    // Sort by lo increasing, hi decreasing to break ties.
    qsortIntPair(r, 0, len - 2)

    // Merge abutting, overlapping.
    var w: Int = 2 // write index
    var i: Int = 2
    while (i < len) {
      val lo: Int = r(i)
      val hi: Int = r(i + 1)
      if (lo <= r(w - 1) + 1) {
        // merge with previous range
        if (hi > r(w - 1)) {
          r(w - 1) = hi
        }
      } else {
        // new disjoint range
        r(w) = lo
        r(w + 1) = hi
        w += 2
      }
      i += 2
    }
    len = w

    this
  }

  // appendLiteral() appends the literal |x| to this CharClass.
  def appendLiteral(x: Int, flags: Int): CharClass = {
    if ((flags & RE2.FOLD_CASE) != 0) {
      appendFoldedRange(x, x)
    } else {
      appendRange(x, x)
    }
  }

  // appendRange() appends the range [lo-hi] (inclusive) to this CharClass.
  def appendRange(lo: Int, hi: Int): CharClass = {
    // Expand last range or next to last range if it overlaps or abuts.
    // Checking two ranges helps when appending case-folded
    // alphabets, so that one range can be expanding A-Z and the
    // other expanding a-z.
    if (len > 0) {
      var i: Int = 2
      while (i <= 4) {
        if (len >= i) {
          val rlo: Int = r(len - i)
          val rhi: Int = r(len - i + 1)
          if (lo <= rhi + 1 && rlo <= hi + 1) {
            if (lo < rlo) {
              r(len - i) = lo
            }
            if (hi > rhi) {
              r(len - i + 1) = hi
            }
            return this
          }
        }
        i += 2
      }
    }

    // Can't coalesce append.   Expand capacity by doubling as needed.
    ensureCapacity(len + 2)
    r(len) = lo
    len += 1
    r(len) = hi
    len += 1

    this
  }

  // appendFoldedRange() appends the range [lo-hi] and its case
  // folding-equivalent runes to this CharClass.
  def appendFoldedRange(_lo: Int, _hi: Int): CharClass = {
    var lo: Int = _lo
    var hi: Int = _hi

    // Optimizations.
    if (lo <= Unicode.MIN_FOLD && hi >= Unicode.MAX_FOLD) {
      // Range is full: folding can't add more.
      return appendRange(lo, hi)
    }
    if (hi < Unicode.MIN_FOLD || lo > Unicode.MAX_FOLD) {
      // Range is outside folding possibilities.
      return appendRange(lo, hi)
    }
    if (lo < Unicode.MIN_FOLD) {
      // [lo, minFold-1] needs no folding.
      appendRange(lo, Unicode.MIN_FOLD - 1)
      lo = Unicode.MIN_FOLD
    }
    if (hi > Unicode.MAX_FOLD) {
      // [maxFold+1, hi] needs no folding.
      appendRange(Unicode.MAX_FOLD + 1, hi)
      hi = Unicode.MAX_FOLD
    }

    // Brute force.  Depend on appendRange to coalesce ranges on the fly.
    var c: Int = lo
    while (c <= hi) {
      appendRange(c, c)
      var f: Int = Unicode.simpleFold(c)
      while (f != c) {
        appendRange(f, f)
        f = Unicode.simpleFold(f)
      }
      c += 1
    }

    this
  }

  // appendClass() appends the class |x| to this CharClass.
  // It assumes |x| is clean.  Does not mutate |x|.
  def appendClass(x: Array[Int]): CharClass = {
    var i: Int = 0
    while (i < x.length) {
      appendRange(x(i), x(i + 1))
      i += 2
    }

    this
  }

  // appendFoldedClass() appends the case folding of the class |x| to this
  // CharClass.  Does not mutate |x|.
  def appendFoldedClass(x: Array[Int]): CharClass = {
    var i: Int = 0
    while (i < x.length) {
      appendFoldedRange(x(i), x(i + 1))
      i += 2
    }

    this
  }

  // appendNegatedClass() append the negation of the class |x| to this
  // CharClass.  It assumes |x| is clean.  Does not mutate |x|.
  def appendNegatedClass(x: Array[Int]): CharClass = {
    var nextLo: Int = 0
    var i: Int      = 0
    while (i < x.length) {
      val lo: Int = x(i)
      val hi: Int = x(i + 1)
      if (nextLo <= lo - 1) {
        appendRange(nextLo, lo - 1)
      }
      nextLo = hi + 1
      i += 2
    }
    if (nextLo <= Unicode.MAX_RUNE) {
      appendRange(nextLo, Unicode.MAX_RUNE)
    }

    this
  }

  // appendTable() appends the Unicode range table |table| to this CharClass.
  // Does not mutate |table|.
  def appendTable(table: Array[Array[Int]]): CharClass = {
    var i: Int = 0
    while (i <= table.length) {
      val triple: Array[Int] = table(i)
      val lo: Int     = triple(0)
      val hi: Int     = triple(1)
      val stride: Int = triple(2)
      if (stride == 1) {
        appendRange(lo, hi)
      } else {
        var c: Int = lo
        while (c <= hi) {
          appendRange(c, c)
          c += stride
        }
      }
      i += 1
    }

    this
  }

  // appendNegatedTable() returns the result of appending the negation of range
  // table |table| to this CharClass.  Does not mutate |table|.
  def appendNegatedTable(table: Array[Array[Int]]): CharClass = {
    var nextLo: Int = 0 // lo end of next class to add
    var i: Int      = 0
    while (i <= table.length) {
      val triple: Array[Int] = table(i)
      val lo: Int     = triple(0)
      val hi: Int     = triple(1)
      val stride: Int = triple(2)
      if (stride == 1) {
        if (nextLo <= lo - 1) {
          appendRange(nextLo, lo - 1)
        }
        nextLo = hi + 1
      } else {
        var c: Int = lo
        while (c <= hi) {
          if (nextLo <= c - 1) {
            appendRange(nextLo, c - 1)
          }
          nextLo = c + 1
          c += stride
        }
      }
      i += 1
    }
    if (nextLo <= Unicode.MAX_RUNE) {
      appendRange(nextLo, Unicode.MAX_RUNE)
    }

    this
  }

  // appendTableWithSign() calls append{,Negated}Table depending on sign.
  // Does not mutate |table|.
  def appendTableWithSign(table: Array[Array[Int]], sign: Int): CharClass = {
    if (sign < 0) {
      appendNegatedTable(table)
    } else {
      appendTable(table)
    }
  }

  // negateClass() negates this CharClass, which must already be clean.
  def negateClass(): CharClass = {
    var nextLo: Int = 0 // lo end of next class to add
    var w: Int      = 0 // write index
    var i: Int      = 0
    while (i < len) {
      val lo: Int = r(i)
      val hi: Int = r(i + 1)
      if (nextLo <= lo - 1) {
        r(w) = nextLo
        r(w + 1) = lo - 1
        w += 2
      }
      nextLo = hi + 1
      i += 2
    }
    len = w

    if (nextLo <= Unicode.MAX_RUNE) {
      // It's possible for the negation to have one more
      // range - this one - than the original class, so use append.
      ensureCapacity(len + 2)
      r(len) = nextLo
      len += 1
      r(len) = Unicode.MAX_RUNE
      len += 1
    }

    this
  }

  // appendClassWithSign() calls appendClass() if sign is +1 or
  // appendNegatedClass if sign is -1.  Does not mutate |x|.
  def appendClassWithSign(x: Array[Int], sign: Int): CharClass = {
    if (sign < 0) {
      appendNegatedClass(x)
    } else {
      appendClass(x)
    }
  }

  // appendGroup() appends CharGroup |g| to this CharClass, folding iff
  // |foldCase|.  Does not mutate |g|.
  def appendGroup(g: CharGroup, foldCase: Boolean): CharClass = {
    var cls: Array[Int] = g.cls
    if (foldCase) {
      cls = new CharClass(Utils.EMPTY_INTS).appendFoldedClass(cls).cleanClass().toArray()
    }

    appendClassWithSign(cls, g.sign)
  }

  override def toString(): String =
    charClassToString(r, len)
}

object CharClass {

  // cmp() returns the ordering of the pair (a(i), a(i+1)) relative to
  // (pivotFrom, pivotTo), where the first component of the pair (lo) is
  // ordered naturally and the second component (hi) is in reverse order.
  private def cmp(array: Array[Int],
                          i: Int,
                          pivotFrom: Int,
                          pivotTo: Int): Int = {
    val cmp: Int = array(i) - pivotFrom

    if (cmp != 0) cmp else pivotTo - array(i + 1)
  }

  // qsortIntPair() quicksorts pairs of ints in |array| according to lt().
  // Precondition: |left|, |right|, |this.len| must all be even |this.len > 1|.
  private def qsortIntPair(array: Array[Int], left: Int, right: Int): Unit = {
    val pivotIndex: Int = ((left + right) / 2) & ~1
    val pivotFrom: Int  = array(pivotIndex)
    val pivotTo: Int    = array(pivotIndex + 1)
    var i: Int          = left
    var j: Int          = right

    while (i <= j) {
      while (i < right && cmp(array, i, pivotFrom, pivotTo) < 0) {
        i += 2
      }
      while (j > left && cmp(array, j, pivotFrom, pivotTo) > 0) {
        j -= 2
      }
      if (i <= j) {
        if (i != j) {
          var temp: Int = array(i)
          array(i) = array(j)
          array(j) = temp
          temp = array(i + 1)
          array(i + 1) = array(j + 1)
          array(j + 1) = temp
        }
        i += 2
        j -= 2
      }
    }
    if (left < j) {
      qsortIntPair(array, left, j)
    }
    if (i < right) {
      qsortIntPair(array, i, right)
    }
  }

  // Exposed, since useful for debugging CharGroups too.
  def charClassToString(r: Array[Int], len: Int): String = {
    val b: java.lang.StringBuilder = new java.lang.StringBuilder()
    b.append('[')
    var i: Int = 0
    while (i < len) {
      if (i > 0) {
        b.append(' ')
      }
      val lo: Int = r(i)
      val hi: Int = r(i + 1)
      // Avoid String.format (not available on GWT).
      // Cf. https://code.google.com/p/google-web-toolkit/issues/detail?id=3945
      if (lo == hi) {
        b.append("0x")
        b.append(Integer.toHexString(lo))
      } else {
        b.append("0x")
        b.append(Integer.toHexString(lo))
        b.append("-0x")
        b.append(Integer.toHexString(hi))
      }
      i += 2
    }
    b.append(']')
    b.toString()
  }
}
