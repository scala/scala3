// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/prog.go

package java.util.regex

import java.util.regex.Inst._
import java.util.regex.Inst.Op._

/**
 * A single instruction in the regular expression virtual machine.
 * @see http://swtch.com/~rsc/regexp/regexp2.html
 */
class Inst(var op: Inst.Op) {
  var out: Int          = 0 // all but MATCH, FAIL
  var arg: Int          = 0 // ALT, ALT_MATCH, CAPTURE, EMPTY_WIDTH
  var runes: Array[Int] = null // length==1 => exact match
  // otherwise a list of [lo,hi] pairs.  hi is *inclusive*.

  // op() returns i.Op but merges all the rune special cases into RUNE
  // Beware "op" is a public field.
  def runeOp(): Op = {
    op match {
      case RUNE1 | RUNE_ANY | RUNE_ANY_NOT_NL =>
        Op.RUNE
      case _ =>
        op
    }
  }

  // MatchRune returns true if the instruction matches (and consumes) r.
  // It should only be called when op == InstRune.
  def matchRune(r: Int): Boolean = {
    // Special case: single-rune slice is from literal string, not char
    // class.
    if (runes.length == 1) {
      val r0: Int = runes(0)
      if (r == r0) {
        return true
      }
      if ((arg & RE2.FOLD_CASE) != 0) {
        var r1: Int = Unicode.simpleFold(r0)
        while (r1 != r0) {
          if (r == r1) {
            return true
          }
          r1 = Unicode.simpleFold(r1)
        }
      }
      return false
    }

    // Peek at the first few pairs.
    // Should handle ASCII well.
    var j: Int = 0
    while (j < runes.length && j <= 8) {
      if (r < runes(j)) {
        return false
      }
      if (r <= runes(j + 1)) {
        return true
      }
      j += 2
    }

    // Otherwise binary search.
    var lo: Int = 0
    var hi: Int = runes.length / 2
    while (lo < hi) {
      val m: Int = lo + (hi - lo) / 2
      val c: Int = runes(2 * m)
      if (c <= r) {
        if (r <= runes(2 * m + 1)) {
          return true
        }
        lo = m + 1
      } else {
        hi = m
      }
    }

    false
  }

  override def toString(): String = {
    op match {
      case ALT =>
        "alt -> " + out + ", " + arg
      case ALT_MATCH =>
        "altmatch -> " + out + ", " + arg
      case CAPTURE =>
        "cap " + arg + " -> " + out
      case EMPTY_WIDTH =>
        "empty " + arg + " -> " + out
      case MATCH =>
        "match"
      case FAIL =>
        "fail"
      case NOP =>
        "nop -> " + out
      case RUNE =>
        if (runes == null) {
          "rune <null>" // can't happen
        } else {
          "rune " + escapeRunes(runes) +
            (if ((arg & RE2.FOLD_CASE) != 0) "/i" else "") + " -> " + out
        }
      case RUNE1 =>
        "rune1 " + escapeRunes(runes) + " -> " + out
      case RUNE_ANY =>
        "any -> " + out
      case RUNE_ANY_NOT_NL =>
        "anynotnl -> " + out
      case _ =>
        throw new IllegalStateException("unhandled case in Inst.toString")
    }
  }
}

object Inst {
  type Op = Int
  object Op {
    final val ALT: Int             = 0
    final val ALT_MATCH: Int       = 1
    final val CAPTURE: Int         = 2
    final val EMPTY_WIDTH: Int     = 3
    final val FAIL: Int            = 4
    final val MATCH: Int           = 5
    final val NOP: Int             = 6
    final val RUNE: Int            = 7
    final val RUNE1: Int           = 8
    final val RUNE_ANY: Int        = 9
    final val RUNE_ANY_NOT_NL: Int = 10
  }

  // Returns an RE2 expression matching exactly |runes|.
  private def escapeRunes(runes: Array[Int]): String = {
    val out: java.lang.StringBuilder = new java.lang.StringBuilder()
    out.append('"')
    var i: Int = 0
    while (i < runes.length) {
      val rune: Int = runes(i)
      Utils.escapeRune(out, rune)
    }
    out.append('"')
    out.toString
  }
}
