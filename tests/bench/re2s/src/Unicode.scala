// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Many of these were derived from the corresponding Go functions in
// http://code.google.com/p/go/source/browse/src/pkg/unicode/letter.go

package java.util.regex

/**
 * Utilities for dealing with Unicode better than Java does.
 *
 * @author adonovan@google.com (Alan Donovan)
 */
object Unicode {

  // The highest legal rune value.
  final val MAX_RUNE: Int = 0x10FFFF

  // The highest legal ASCII value.
  final val MAX_ASCII: Int = 0x7f

  // The highest legal Latin-1 value.
  final val MAX_LATIN1: Int = 0xFF

  private final val MAX_CASE: Int = 3

  // Represents invalid code points.
  private final val REPLACEMENT_CHAR: Int = 0xFFFD

  // Minimum and maximum runes involved in folding.
  // Checked during test.
  final val MIN_FOLD: Int = 0x0041
  final val MAX_FOLD: Int = 0x1044f

  // is32 uses binary search to test whether rune is in the specified
  // slice of 32-bit ranges.
  // TODO(adonovan): opt: consider using int[n*3] instead of int[n][3].
  private def is32(ranges: Array[Array[Int]], r: Int): Boolean = {
    // binary search over ranges
    var lo: Int = 0
    var hi: Int = ranges.length
    while (lo < hi) {
      val m: Int     = lo + (hi - lo) / 2
      val range: Array[Int] = ranges(m) // [lo, hi, stride]
      if (range(0) <= r && r <= range(1)) {
        return ((r - range(0)) % range(2)) == 0
      }
      if (r < range(0)) {
        hi = m
      } else {
        lo = m + 1
      }
    }
    return false
  }

  // is tests whether rune is in the specified table of ranges.
  private def is(ranges: Array[Array[Int]], r: Int): Boolean = {
    // common case: rune is ASCII or Latin-1, so use linear search.
    if (r <= MAX_LATIN1) {
      var i: Int = 0
      while (i < ranges.length) {
        val range: Array[Int] = ranges(i) // range = [lo, hi, stride]
        if (r > range(1)) {
          i += 1
        } else {
          if (r < range(0)) {
            return false
          }
          return ((r - range(0)) % range(2)) == 0
        }
      }
      return false
    }

    ranges.length > 0 && r >= ranges(0)(0) && is32(ranges, r)
  }

  // isUpper reports whether the rune is an upper case letter.
  def isUpper(r: Int): Boolean = {
    // See comment in isGraphic.
    if (r <= MAX_LATIN1) {
      Character.isUpperCase(r.toChar)
    } else {
      is(UnicodeTables.Upper, r)
    }
  }

  // isLower reports whether the rune is a lower case letter.
  def isLower(r: Int): Boolean = {
    // See comment in isGraphic.
    if (r <= MAX_LATIN1) {
      Character.isLowerCase(r.toChar)
    } else {
      is(UnicodeTables.Lower, r)
    }
  }

  // isTitle reports whether the rune is a title case letter.
  def isTitle(r: Int): Boolean = {
    if (r <= MAX_LATIN1) {
      false
    } else {
      is(UnicodeTables.Title, r)
    }
  }

  // isPrint reports whether the rune is printable (Unicode L/M/N/P/S or ' ').
  def isPrint(r: Int): Boolean = {
    if (r <= MAX_LATIN1) {
      r >= 0x20 && r < 0x7F ||
      r >= 0xA1 && r != 0xAD
    } else {
      is(UnicodeTables.L, r) ||
      is(UnicodeTables.M, r) ||
      is(UnicodeTables.N, r) ||
      is(UnicodeTables.P, r) ||
      is(UnicodeTables.S, r)
    }
  }

  // A case range is conceptually a record:
  // class CaseRange {
  //   int lo, hi
  //   int upper, lower, title
  // }
  // but flattened as an int[5].

  // to maps the rune using the specified case mapping.
  private def to_3(kase: Int, r: Int, caseRange: Array[Array[Int]]): Int = {
    if (kase < 0 || MAX_CASE <= kase) {
      return REPLACEMENT_CHAR // as reasonable an error as any
    }
    // binary search over ranges
    var lo: Int = 0
    var hi: Int = caseRange.length
    while (lo < hi) {
      val m: Int    = lo + (hi - lo) / 2
      val cr: Array[Int]   = caseRange(m) // cr = [lo, hi, upper, lower, title]
      val crlo: Int = cr(0)
      val crhi: Int = cr(1)
      if (crlo <= r && r <= crhi) {
        val delta: Int = cr(2 + kase)
        if (delta > MAX_RUNE) {
          // In an Upper-Lower sequence, which always starts with
          // an UpperCase letter, the real deltas always look like:
          //      {0, 1, 0}    UpperCase (Lower is next)
          //      {-1, 0, -1}  LowerCase (Upper, Title are previous)
          // The characters at even offsets from the beginning of the
          // sequence are upper case the ones at odd offsets are lower.
          // The correct mapping can be done by clearing or setting the low
          // bit in the sequence offset.
          // The constants UpperCase and TitleCase are even while LowerCase
          // is odd so we take the low bit from kase.
          return crlo + (((r - crlo) & ~1) | (kase & 1))
        }
        return r + delta
      }
      if (r < crlo) {
        hi = m
      } else {
        lo = m + 1
      }
    }
    return r
  }

  // to maps the rune to the specified case: UpperCase, LowerCase, or TitleCase.
  private def to_2(kase: Int, r: Int): Int =
    to_3(kase, r, UnicodeTables.CASE_RANGES)

  // toUpper maps the rune to upper case.
  def toUpper(r: Int): Int = {
    if (r <= MAX_ASCII) {
      var res: Int = r
      if ('a' <= r && r <= 'z') {
        res -= 'a' - 'A'
      }
      res
    } else {
      to_2(UnicodeTables.UpperCase, r)
    }
  }

  // toLower maps the rune to lower case.
  def toLower(r: Int): Int = {
    if (r <= MAX_ASCII) {
      var res: Int = r
      if ('A' <= r && r <= 'Z') {
        res += 'a' - 'A'
      }
      res
    } else {
      to_2(UnicodeTables.LowerCase, r)
    }
  }

  // simpleFold iterates over Unicode code points equivalent under
  // the Unicode-defined simple case folding.  Among the code points
  // equivalent to rune (including rune itself), SimpleFold returns the
  // smallest r >= rune if one exists, or else the smallest r >= 0.
  //
  // For example:
  //      SimpleFold('A') = 'a'
  //      SimpleFold('a') = 'A'
  //
  //      SimpleFold('K') = 'k'
  //      SimpleFold('k') = '\u212A' (Kelvin symbol, K)
  //      SimpleFold('\u212A') = 'K'
  //
  //      SimpleFold('1') = '1'
  //
  // Derived from Go's unicode.SimpleFold.
  //
  def simpleFold(r: Int): Int = {
    // Consult caseOrbit table for special cases.
    var lo: Int = 0
    var hi: Int = UnicodeTables.CASE_ORBIT.length
    while (lo < hi) {
      val m: Int = lo + (hi - lo) / 2
      if (UnicodeTables.CASE_ORBIT(m)(0) < r) {
        lo = m + 1
      } else {
        hi = m
      }
    }
    if (lo < UnicodeTables.CASE_ORBIT.length &&
        UnicodeTables.CASE_ORBIT(lo)(0) == r) {
      return UnicodeTables.CASE_ORBIT(lo)(1)
    }

    // No folding specified.  This is a one- or two-element
    // equivalence class containing rune and toLower(rune)
    // and toUpper(rune) if they are different from rune.
    val l: Int = toLower(r)
    if (l != r) {
      return l
    }
    return toUpper(r)
  }
}
