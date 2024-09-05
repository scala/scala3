// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/regexp.go

package java.util.regex

import java.util.Arrays
import java.util.regex.Regexp._

/**
 * Regular expression abstract syntax tree.
 * Produced by parser, used by compiler.
 * NB, this corresponds to {@code syntax.regexp} in the Go implementation
 * Go's {@code regexp} is called {@code RE2} in Java.
 */
class Regexp() {
  var op: Op              = 0 // operator
  var flags: Int          = 0 // bitmap of parse flags
  var subs: Array[Regexp] = null // subexpressions, if any.  Never null.
  // subs[0] is used as the freelist.
  var runes: Array[Int] = null // matched runes, for LITERAL, CHAR_CLASS
  var min: Int     = 0 // min, max for REPEAT
  var max: Int     = 0 // min, max for REPEAT
  var cap: Int          = 0 // capturing index, for CAPTURE
  var name: String      = null // capturing name, for CAPTURE

  def reinit(): Unit = {
    this.flags = 0
    subs = EMPTY_SUBS
    runes = null
    min = 0
    max = 0
    cap = 0
    name = null
  }

  override def toString(): String = {
    val out: java.lang.StringBuilder = new java.lang.StringBuilder()
    appendTo(out)
    out.toString
  }

  // appendTo() appends the Perl syntax for |this| regular expression to |out|.
  private def appendTo(out: java.lang.StringBuilder): Unit = {
    op match {
      case Op.NO_MATCH =>
        out.append("[^\\x00-\\x{10FFFF}]")
      case Op.EMPTY_MATCH =>
        out.append("(?:)")
      case Op.STAR | Op.PLUS | Op.QUEST | Op.REPEAT =>
        val sub: Regexp = subs(0)
        if (sub.op > Op.CAPTURE ||
            sub.op == Op.LITERAL && sub.runes.length > 1) {
          out.append("(?:")
          sub.appendTo(out)
          out.append(')')
        } else {
          sub.appendTo(out)
        }
        op match {
          case Op.STAR =>
            out.append('*')
          case Op.PLUS =>
            out.append('+')
          case Op.QUEST =>
            out.append('?')
          case Op.REPEAT =>
            out.append('{').append(min)
            if (min != max) {
              out.append(',')
              if (max >= 0) {
                out.append(max)
              }
            }
            out.append('}')
        }
        if ((flags & RE2.NON_GREEDY) != 0) {
          out.append('?')
        }
      case Op.CONCAT =>
        var i: Int = 0
        while (i <= subs.length) {
          val sub: Regexp = subs(i)
          if (sub.op == Op.ALTERNATE) {
            out.append("(?:")
            sub.appendTo(out)
            out.append(')')
          } else {
            sub.appendTo(out)
          }
          i += 1
        }
      case Op.ALTERNATE =>
        var sep: String = ""
        var i: Int   = 0
        while (i <= subs.length) {
          val sub: Regexp = subs(i)
          out.append(sep)
          sep = "|"
          sub.appendTo(out)
          i += 1
        }
      case Op.LITERAL =>
        if ((flags & RE2.FOLD_CASE) != 0) {
          out.append("(?i:")
        }
        var i: Int = 0
        while (i < runes.length) {
          val rune: Int = runes(i)
          Utils.escapeRune(out, rune)
          i += 1
        }
        if ((flags & RE2.FOLD_CASE) != 0) {
          out.append(')')
        }
      case Op.ANY_CHAR_NOT_NL =>
        out.append("(?-s:.)")
      case Op.ANY_CHAR =>
        out.append("(?s:.)")
      case Op.CAPTURE =>
        if (name == null || name.isEmpty()) {
          out.append('(')
        } else {
          out.append("(?P<")
          out.append(name)
          out.append(">")
        }
        if (subs(0).op != Op.EMPTY_MATCH) {
          subs(0).appendTo(out)
        }
        out.append(')')
      case Op.BEGIN_TEXT =>
        out.append("\\A")
      case Op.END_TEXT =>
        if ((flags & RE2.WAS_DOLLAR) != 0) {
          out.append("(?-m:$)")
        } else {
          out.append("\\z")
        }
      case Op.BEGIN_LINE =>
        out.append('^')
      case Op.END_LINE =>
        out.append('$')
      case Op.WORD_BOUNDARY =>
        out.append("\\b")
      case Op.NO_WORD_BOUNDARY =>
        out.append("\\B")
      case Op.CHAR_CLASS =>
        if (runes.length % 2 != 0) {
          out.append("[invalid char class]")
        } else {
          out.append('[')
          if (runes.length == 0) {
            out.append("^\\x00-\\x{10FFFF}")
          } else if (runes(0) == 0 &&
                     runes(runes.length - 1) == Unicode.MAX_RUNE) {
            // Contains 0 and MAX_RUNE.  Probably a negated class.
            // Print the gaps.
            out.append('^')
            var i: Int = 1
            while (i < runes.length - 1) {
              val lo: Int = runes(i) + 1
              val hi: Int = runes(i + 1) - 1
              quoteIfHyphen(out, lo)
              Utils.escapeRune(out, lo)
              if (lo != hi) {
                out.append('-')
                quoteIfHyphen(out, hi)
                Utils.escapeRune(out, hi)
              }
              i += 2
            }
          } else {
            var i: Int = 0
            while (i < runes.length) {
              val lo: Int = runes(i)
              val hi: Int = runes(i + 1)
              quoteIfHyphen(out, lo)
              Utils.escapeRune(out, lo)
              if (lo != hi) {
                out.append('-')
                quoteIfHyphen(out, hi)
                Utils.escapeRune(out, hi)
              }
              i += 2
            }
          }
          out.append(']')
        }
      case _ => // incl. pseudos
        out.append(op)
    }
  }

  // maxCap() walks the regexp to find the maximum capture index.
  def maxCap(): Int = {
    var m: Int = 0
    if (op == Op.CAPTURE) {
      m = cap
    }
    if (subs != null) {
      var i: Int = 0
      while (i < subs.length) {
        val sub: Regexp = subs(i)
        val n: Int   = sub.maxCap()
        if (m < n) {
          m = n
        }
        i += 1
      }
    }
    m
  }

  // equals() returns true if this and that have identical structure.
  override def equals(that: Any): Boolean = {
    that match {
      case that: Regexp =>
        val x: Regexp = this
        val y: Regexp = that
        if (x.op != y.op) {
          return false
        }
        x.op match {
          case Op.END_TEXT =>
            // The parse flags remember whether this is \z or \Z.
            if ((x.flags & RE2.WAS_DOLLAR) != (y.flags & RE2.WAS_DOLLAR)) {
              return false
            }
          case Op.LITERAL | Op.CHAR_CLASS =>
            if (!Arrays.equals(x.runes, y.runes)) {
              return false
            }
          case Op.ALTERNATE | Op.CONCAT =>
            if (x.subs.length != y.subs.length) {
              return false
            }
            var i: Int = 0
            while (i < x.subs.length) {
              if (!x.subs(i).equals(y.subs(i))) {
                return false
              }
              i += 1
            }
          case Op.STAR | Op.PLUS | Op.QUEST =>
            if ((x.flags & RE2.NON_GREEDY) != (y.flags & RE2.NON_GREEDY) ||
                !x.subs(0).equals(y.subs(0))) {
              return false
            }
          case Op.REPEAT =>
            if ((x.flags & RE2.NON_GREEDY) != (y.flags & RE2.NON_GREEDY) ||
                x.min != y.min || x.max != y.max || !x.subs(0).equals(y.subs(0))) {
              return false
            }
          case Op.CAPTURE =>
            if (x.cap != y.cap || x.name != y.name ||
                !x.subs(0).equals(y.subs(0))) {
              return false
            }
        }
        return true
      case _ =>
        false
    }
  }

}

object Regexp {
  type Op = Int
  object Op {
    final val NO_MATCH: Int         = 0  // Matches no strings.
    final val EMPTY_MATCH: Int      = 1  // Matches empty string.
    final val LITERAL: Int          = 2  // Matches runes[] sequence
    final val CHAR_CLASS: Int       = 3  // Matches Runes interpreted as range pair list
    final val ANY_CHAR_NOT_NL: Int  = 4  // Matches any character except '\n'
    final val ANY_CHAR: Int         = 5  // Matches any character
    final val BEGIN_LINE: Int       = 6  // Matches empty string at end of line
    final val END_LINE: Int         = 7  // Matches empty string at end of line
    final val BEGIN_TEXT: Int       = 8  // Matches empty string at beginning of text
    final val END_TEXT: Int         = 9  // Matches empty string at end of text
    final val WORD_BOUNDARY: Int    = 10 // Matches word boundary `\b`
    final val NO_WORD_BOUNDARY: Int = 11 // Matches word non-boundary `\B`
    final val CAPTURE: Int          = 12 // Capturing subexpr with index cap, optional name name
    final val STAR: Int             = 13 // Matches subs[0] zero or more times.
    final val PLUS: Int             = 14 // Matches subs[0] one or more times.
    final val QUEST: Int            = 15 // Matches subs[0] zero or one times.
    final val REPEAT: Int           = 16 // Matches subs[0] [min, max] times max=-1 => no limit.
    final val CONCAT: Int           = 17 // Matches concatenation of subs[]
    final val ALTERNATE: Int        = 18 // Matches union of subs[]

    // Pseudo ops, used internally by Parser for parsing stack:
    final val LEFT_PAREN: Int   = 19
    final val VERTICAL_BAR: Int = 20

    def isPseudo(op: Op): Boolean = op >= LEFT_PAREN
  }

  final val EMPTY_SUBS: Array[Regexp] = new Array[Regexp](0)

  private def quoteIfHyphen(out: java.lang.StringBuilder, rune: Int): Unit = {
    if (rune == '-') {
      out.append('\\')
    }
  }
}
