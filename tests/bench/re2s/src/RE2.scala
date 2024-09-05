// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/regexp.go

// Beware, submatch results may pin a large underlying String into
// memory.  Consider creating explicit string copies if submatches are
// long-lived and inputs are large.
//
// The JDK API supports incremental processing of the input without
// necessarily consuming it all we do not attempt to do so.

// The Java API emphasises UTF-16 Strings, not UTF-8 byte[] as in Go, as
// the primary input datatype, and the method names have been changed to
// reflect this.

package java.util.regex

import java.util.ArrayList
import java.util.Arrays
import java.util.List
import java.util.regex.RE2._

/**
 * An RE2 class instance is a compiled representation of an RE2 regular
 * expression, independent of the public Java-like Pattern/Matcher API.
 *
 * <p>This class also contains various implementation helpers for RE2
 * regular expressions.
 *
 * <p>Use the {@link #quoteMeta(String)} utility function to quote all
 * regular expression metacharacters in an arbitrary string.
 *
 * <p>See the {@code Matcher} and {@code Pattern} classes for the public
 * API, and the <a href='package.html'>package-level
 * documentation</a> for an overview of how to use this API.
 */
class RE2() {
  var expr: String = null // as passed to Compile
  var prog: Prog   = null // compiled program
  var cond: Int    = 0 // EMPTY_* bitmask: empty-width conditions
  // required at start of match
  var numSubexp: Int   = 0
  var longest: Boolean = false

  var prefix: String = null // required UTF-16 prefix in unanchored matches
  var prefixUTF8
    : Array[Byte]             = null // required UTF-8 prefix in unanchored matches
  var prefixComplete: Boolean = false // true iff prefix is the entire regexp
  var prefixRune: Int         = 0 // first rune in prefix

  // Cache of machines for running regexp.
  // Accesses must be serialized using |this| monitor.
  private val machine: ArrayList[Machine] = new ArrayList[Machine]()

  /**
   * Returns the number of parenthesized subexpressions in this regular
   * expression.
   */
  def numberOfCapturingGroups(): Int = numSubexp

  // get() returns a machine to use for matching |this|.  It uses |this|'s
  // machine cache if possible, to avoid unnecessary allocation.
  def get(): Machine = synchronized[Machine]({
    val n: Int = machine.size()
    if (n > 0) {
      return machine.remove(n - 1)
    }
    return new Machine(this)
  })

  // Clears the memory associated with this machine.
  def reset_0(): Unit = synchronized[Unit]({
    machine.clear()
  })

  // put() returns a machine to |this|'s machine cache.  There is no attempt to
  // limit the size of the cache, so it will grow to the maximum number of
  // simultaneous matches run using |this|.  (The cache empties when |this|
  // gets garbage collected.)
  def put(m: Machine): Unit = synchronized[Unit]({
    machine.add(m)
  })

  override def toString(): String = expr

  // doExecute() finds the leftmost match in the input and returns
  // the position of its subexpressions.
  // Derived from exec.go.
  private def doExecute(in: MachineInput,
                        pos: Int,
                        anchor: Int,
                        ncap: Int): Array[Int] = {
    val m: Machine = get()
    m.init(ncap)
    val cap: Array[Int] = if (m.match_(in, pos, anchor)) m.submatches() else null
    put(m)
    cap
  }

  /**
   * Returns true iff this regexp matches the string {@code s}.
   */
  def match_1(s: CharSequence): Boolean =
    doExecute(MachineInput.fromUTF16(s, 0, s.length()), 0, UNANCHORED, 0) != null

  /**
   * Matches the regular expression against input starting at position start
   * and ending at position end, with the given anchoring.
   * Records the submatch boundaries in group, which is [start, end) pairs
   * of byte offsets. The number of boundaries needed is inferred
   * from the size of the group array. It is most efficient not to ask for
   * submatch boundaries.
   *
   * @param input the input byte array
   * @param start the beginning position in the input
   * @param end the end position in the input
   * @param anchor the anchoring flag (UNANCHORED, ANCHOR_START, ANCHOR_BOTH)
   * @param group the array to fill with submatch positions
   * @param ngroup the number of array pairs to fill in
   * @return true if a match was found
   */
  def match_5(input: CharSequence,
             start: Int,
             end: Int,
             anchor: Int,
             group: Array[Int],
             ngroup: Int): Boolean = {
    if (start > end) {
      return false
    }

    val groupMatch: Array[Int] = doExecute(MachineInput.fromUTF16(input, 0, end),
                               start,
                               anchor,
                               2 * ngroup)

    if (groupMatch == null) {
      return false
    }

    if (group != null) {
      System.arraycopy(groupMatch, 0, group, 0, groupMatch.length)
    }

    true
  }

  /**
   * Returns true iff this regexp matches the UTF-8 byte array {@code b}.
   */
  // This is visible for testing.
  def matchUTF8(b: Array[Byte]): Boolean =
    doExecute(MachineInput.fromUTF8(b, 0, b.length), 0, UNANCHORED, 0) != null

  /**
   * Returns a copy of {@code src} in which all matches for this regexp
   * have been replaced by {@code repl}.  No support is provided for
   * expressions (e.g. {@code \1} or {@code $1}) in the replacement
   * string.
   */
  // This is visible for testing.
  def replaceAll(src: String, repl: String): String =
    replaceAllFunc(src, 2 * src.length() + 1, (orig: String) => {
      repl
    })

  /**
   * Returns a copy of {@code src} in which only the first match for this regexp
   * has been replaced by {@code repl}.  No support is provided for
   * expressions (e.g. {@code \1} or {@code $1}) in the replacement
   * string.
   */
  // This is visible for testing.
  def replaceFirst(src: String, repl: String): String =
    replaceAllFunc(src, 1, (orig: String) => {
      repl
    })

  /**
   * Returns a copy of {@code src} in which at most {@code maxReplaces} matches
   * for this regexp have been replaced by the return value of of function
   * {@code repl} (whose first argument is the matched string). No support is
   * provided for expressions (e.g. {@code \1} or {@code $1}) in the
   * replacement string.
   */
  // This is visible for testing.
  def replaceAllFunc(src: String, maxReplaces: Int,
      f: String => String): String = {
    var lastMatchEnd: Int = 0 // end position of the most recent match
    var searchPos: Int    = 0 // position where we next look for a match
    val buf: java.lang.StringBuilder          = new java.lang.StringBuilder()
    val input: MachineInput        = MachineInput.fromUTF16(src, 0, src.length())
    var numReplaces: Int  = 0
    var break: Boolean        = false

    while (!break && searchPos <= src.length()) {
      val a: Array[Int] = doExecute(input, searchPos, UNANCHORED, 2)
      if (a == null || a.length == 0) {
        break = true // no more matches
      } else {
        // Copy the unmatched characters before this match.
        buf.append(src.substring(lastMatchEnd, a(0)))

        // Now insert a copy of the replacement string, but not for a
        // match of the empty string immediately after another match.
        // (Otherwise, we get double replacement for patterns that
        // match both empty and nonempty strings.)
        if (a(1) > lastMatchEnd || a(0) == 0) {
          buf.append(f(src.substring(a(0), a(1))))
          // Increment the replace count.
          numReplaces += 1
        }
        lastMatchEnd = a(1)

        // Advance past this match always advance at least one character.
        val width: Int = input.step(searchPos) & 0x7
        if (searchPos + width > a(1)) {
          searchPos += width
        } else if (searchPos + 1 > a(1)) {
          // This clause is only needed at the end of the input
          // string.  In that case, DecodeRuneInString returns width=0.
          searchPos += 1
        } else {
          searchPos = a(1)
        }
        if (numReplaces >= maxReplaces) {
          // Should never be greater though.
          break = true
        }
      }
    }

    // Copy the unmatched characters after the last match.
    buf.append(src.substring(lastMatchEnd))
    buf.toString()
  }

  // The number of capture values in the program may correspond
  // to fewer capturing expressions than are in the regexp.
  // For example, "(a){0}" turns into an empty program, so the
  // maximum capture in the program is 0 but we need to return
  // an expression for \1.  Pad returns a with -1s appended as needed
  // the result may alias a.
  private def pad(_a: Array[Int]): Array[Int] = {
    var a: Array[Int] = _a
    if (a == null) {
      return null // No match.
    }
    var n: Int = (1 + numSubexp) * 2
    if (a.length < n) {
      val a2: Array[Int] = new Array[Int](n)
      System.arraycopy(a, 0, a2, 0, a.length)
      Arrays.fill(a2, a.length, n, -1)
      a = a2
    }
    a
  }

  // Find matches in input.
  private def allMatches(input: MachineInput, _n: Int,
      f: Array[Int] => Unit): Unit = {
    var n: Int   = _n
    val end: Int = input.endPos()
    if (n < 0) {
      n = end + 1
    }
    var pos: Int          = 0
    var i: Int            = 0
    var prevMatchEnd: Int = -1
    var break: Boolean        = false
    while (!break && i < n && pos <= end) {
      val matches: Array[Int] = doExecute(input, pos, UNANCHORED, prog.numCap)
      if (matches == null || matches.length == 0) {
        break = true
      } else {
        var accept: Boolean = true
        if (matches(1) == pos) {
          // We've found an empty match.
          if (matches(0) == prevMatchEnd) {
            // We don't allow an empty match right
            // after a previous match, so ignore it.
            accept = false
          }
          val r: Int = input.step(pos)
          if (r < 0) { // EOF
            pos = end + 1
          } else {
            pos += r & 0x7
          }
        } else {
          pos = matches(1)
        }
        prevMatchEnd = matches(1)

        if (accept) {
          f(pad(matches))
          i += 1
        }
      }
    }
  }

  // Legacy Go-style interface preserved (package-private) for better
  // test coverage.
  //
  // There are 16 methods of RE2 that match a regular expression and
  // identify the matched text.  Their names are matched by this regular
  // expression:
  //
  //    find(All)?(UTF8)?(Submatch)?(Index)?
  //
  // If 'All' is present, the routine matches successive non-overlapping
  // matches of the entire expression.  Empty matches abutting a
  // preceding match are ignored.  The return value is an array
  // containing the successive return values of the corresponding
  // non-All routine.  These routines take an extra integer argument, n
  // if n >= 0, the function returns at most n matches/submatches.
  //
  // If 'UTF8' is present, the argument is a UTF-8 encoded byte[] array
  // otherwise it is a UTF-16 encoded java.lang.String return values
  // are adjusted as appropriate.
  //
  // If 'Submatch' is present, the return value is an list identifying
  // the successive submatches of the expression.  Submatches are
  // matches of parenthesized subexpressions within the regular
  // expression, numbered from left to right in order of opening
  // parenthesis.  Submatch 0 is the match of the entire expression,
  // submatch 1 the match of the first parenthesized subexpression, and
  // so on.
  //
  // If 'Index' is present, matches and submatches are identified by
  // byte index pairs within the input string: result[2*n:2*n+1]
  // identifies the indexes of the nth submatch.  The pair for n==0
  // identifies the match of the entire expression.  If 'Index' is not
  // present, the match is identified by the text of the match/submatch.
  // If an index is negative, it means that subexpression did not match
  // any string in the input.

  /**
   * Returns an array holding the text of the leftmost match in {@code b}
   * of this regular expression.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findUTF8(b: Array[Byte]): Array[Byte] = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF8(b, 0, b.length), 0, UNANCHORED, 2)
    if (a == null) {
      return null
    }
    Utils.subarray_b(b, a(0), a(1))
  }

  /**
   * Returns a two-element array of integers defining the location of
   * the leftmost match in {@code b} of this regular expression.  The
   * match itself is at {@code b[loc[0]...loc[1]]}.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findUTF8Index(b: Array[Byte]): Array[Int] = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF8(b, 0, b.length), 0, UNANCHORED, 2)
    if (a == null) {
      return null
    }
    return Utils.subarray_i(a, 0, 2)
  }

  /**
   * Returns a string holding the text of the leftmost match in
   * {@code s} of this regular expression.
   *
   * <p>If there is no match, the return value is an empty string, but it
   * will also be empty if the regular expression successfully matches
   * an empty string.  Use {@link #findIndex} or
   * {@link #findSubmatch} if it is necessary to distinguish these
   * cases.
   */
  // This is visible for testing.
  def find(s: String): String = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF16(s, 0, s.length()), 0, UNANCHORED, 2)
    if (a == null) {
      return ""
    }
    return s.substring(a(0), a(1))
  }

  /**
   * Returns a two-element array of integers defining the location of
   * the leftmost match in {@code s} of this regular expression.  The
   * match itself is at {@code s.substring(loc[0], loc[1])}.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findIndex(s: String): Array[Int] = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF16(s, 0, s.length()), 0, UNANCHORED, 2)
    if (a == null) {
      return null
    }
    return a
  }

  /**
   * Returns an array of arrays the text of the leftmost match of the
   * regular expression in {@code b} and the matches, if any, of its
   * subexpressions, as defined by the <a
   * href='#submatch'>Submatch</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findUTF8Submatch(b: Array[Byte]): Array[Array[Byte]] = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF8(b, 0, b.length), 0, UNANCHORED, prog.numCap)
    if (a == null) {
      return null
    }
    val ret: Array[Array[Byte]] = new Array[Array[Byte]](1 + numSubexp)
    var i: Int   = 0
    while (i < ret.length) {
      if (2 * i < a.length && a(2 * i) >= 0) {
        ret(i) = Utils.subarray_b(b, a(2 * i), a(2 * i + 1))
      }
      i += 1
    }
    ret
  }

  /**
   * Returns an array holding the index pairs identifying the leftmost
   * match of this regular expression in {@code b} and the matches, if
   * any, of its subexpressions, as defined by the the <a
   * href='#submatch'>Submatch</a> and <a href='#index'>Index</a>
   * descriptions above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findUTF8SubmatchIndex(b: Array[Byte]): Array[Int] =
    pad(doExecute(MachineInput.fromUTF8(b, 0, b.length), 0, UNANCHORED, prog.numCap))

  /**
   * Returns an array of strings holding the text of the leftmost match
   * of the regular expression in {@code s} and the matches, if any, of
   * its subexpressions, as defined by the <a
   * href='#submatch'>Submatch</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findSubmatch(s: String): Array[String] = {
    val a: Array[Int] = doExecute(MachineInput.fromUTF16(s, 0, s.length()), 0, UNANCHORED, prog.numCap)
    if (a == null) {
      return null
    }
    val ret: Array[String] = new Array[String](1 + numSubexp)
    var i: Int   = 0
    while (i < ret.length) {
      if (2 * i < a.length && a(2 * i) >= 0) {
        ret(i) = s.substring(a(2 * i), a(2 * i + 1))
      }
      i += 1
    }
    ret
  }

  /**
   * Returns an array holding the index pairs identifying the leftmost
   * match of this regular expression in {@code s} and the matches, if
   * any, of its subexpressions, as defined by the <a
   * href='#submatch'>Submatch</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findSubmatchIndex(s: String): Array[Int] =
    pad(doExecute(MachineInput.fromUTF16(s, 0, s.length()), 0, UNANCHORED, prog.numCap))

  /**
   * {@code findAllUTF8()} is the <a href='#all'>All</a> version of
   * {@link #findUTF8} it returns a list of up to {@code n} successive
   * matches of the expression, as defined by the <a href='#all'>All</a>
   * description above.
   *
   * <p>A return value of null indicates no match.
   *
   * TODO(adonovan): think about defining a byte slice view class, like
   * a read-only Go slice backed by |b|.
   */
  // This is visible for testing.
  def findAllUTF8(b: Array[Byte], n: Int): List[Array[Byte]] = {
    val result: ArrayList[Array[Byte]] = new ArrayList[Array[Byte]]()
    allMatches(MachineInput.fromUTF8(b, 0, b.length), n, (_match: Array[Int]) => {
      result.add(Utils.subarray_b(b, _match(0), _match(1)))
    })
    if (result.isEmpty()) {
      return null
    }
    result
  }

  /**
   * {@code findAllUTF8Index} is the <a href='#all'>All</a> version of
   * {@link #findUTF8Index} it returns a list of up to {@code n}
   * successive matches of the expression, as defined by the <a
   * href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllUTF8Index(b: Array[Byte], n: Int): List[Array[Int]] = {
    val result: ArrayList[Array[Int]] = new ArrayList[Array[Int]]()
    allMatches(MachineInput.fromUTF8(b, 0, b.length), n, (_match: Array[Int]) => {
      result.add(Utils.subarray_i(_match, 0, 2))
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAll} is the <a href='#all'>All</a> version of
   * {@link #find} it returns a list of up to {@code n}
   * successive matches of the expression, as defined by the <a
   * href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAll(s: String, n: Int): List[String] = {
    val result: ArrayList[String] = new ArrayList[String]()
    allMatches(MachineInput.fromUTF16(s, 0, s.length()), n, (_match: Array[Int]) => {
      result.add(s.substring(_match(0), _match(1)))
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAllIndex} is the <a href='#all'>All</a> version of
   * {@link #findIndex} it returns a list of up to {@code n}
   * successive matches of the expression, as defined by the <a
   * href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllIndex(s: String, n: Int): List[Array[Int]] = {
    val result: ArrayList[Array[Int]] = new ArrayList[Array[Int]]()
    allMatches(MachineInput.fromUTF16(s, 0, s.length()), n, (_match: Array[Int]) => {
      result.add(Utils.subarray_i(_match, 0, 2))
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAllUTF8Submatch} is the <a href='#all'>All</a> version
   * of {@link #findUTF8Submatch} it returns a list of up to {@code n}
   * successive matches of the expression, as defined by the <a
   * href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllUTF8Submatch(b: Array[Byte], n: Int): List[Array[Array[Byte]]] = {
    val result: ArrayList[Array[Array[Byte]]] = new ArrayList[Array[Array[Byte]]]()
    allMatches(MachineInput.fromUTF8(b, 0, b.length), n, (_match: Array[Int]) => {
      val slice: Array[Array[Byte]] = new Array[Array[Byte]](_match.length / 2)
      var j: Int     = 0
      while (j < slice.length) {
        if (_match(2 * j) >= 0) {
          slice(j) = Utils.subarray_b(b, _match(2 * j), _match(2 * j + 1))
        }
        j += 1
      }
      result.add(slice)
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAllUTF8SubmatchIndex} is the <a href='#all'>All</a>
   * version of {@link #findUTF8SubmatchIndex} it returns a list of up
   * to {@code n} successive matches of the expression, as defined by
   * the <a href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllUTF8SubmatchIndex(b: Array[Byte], n: Int): List[Array[Int]] = {
    val result: ArrayList[Array[Int]] = new ArrayList[Array[Int]]()
    allMatches(MachineInput.fromUTF8(b, 0, b.length), n, (_match: Array[Int]) => {
      result.add(_match)
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAllSubmatch} is the <a href='#all'>All</a> version
   * of {@link #findSubmatch} it returns a list of up to
   * {@code n} successive matches of the expression, as defined by the
   * <a href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllSubmatch(s: String, n: Int): List[Array[String]] = {
    val result: ArrayList[Array[String]] = new ArrayList[Array[String]]()
    allMatches(MachineInput.fromUTF16(s, 0, s.length()), n, (_match: Array[Int]) => {
      val slice: Array[String] = new Array[String](_match.length / 2)
      var j: Int     = 0
      while (j < slice.length) {
        if (_match(2 * j) >= 0) {
          slice(j) = s.substring(_match(2 * j), _match(2 * j + 1))
        }
        j += 1
      }
      result.add(slice)
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }

  /**
   * {@code findAllSubmatchIndex} is the <a href='#all'>All</a>
   * version of {@link #findSubmatchIndex} it returns a list of
   * up to {@code n} successive matches of the expression, as defined by
   * the <a href='#all'>All</a> description above.
   *
   * <p>A return value of null indicates no match.
   */
  // This is visible for testing.
  def findAllSubmatchIndex(s: String, n: Int): List[Array[Int]] = {
    val result: ArrayList[Array[Int]] = new ArrayList[Array[Int]]()
    allMatches(MachineInput.fromUTF16(s, 0, s.length()), n, (_match: Array[Int]) => {
      result.add(_match)
    })
    if (result.isEmpty()) {
      return null
    }
    return result
  }
}

object RE2 {

  //// Parser flags.

  // Fold case during matching (case-insensitive).
  final val FOLD_CASE: Int = 0x01

  // Treat pattern as a literal string instead of a regexp.
  final val LITERAL: Int = 0x02

  // Allow character classes like [^a-z] and [[:space:]] to match newline.
  final val CLASS_NL: Int = 0x04

  // Allow '.' to match newline.
  final val DOT_NL: Int = 0x08

  // Treat ^ and $ as only matching at beginning and end of text, not
  // around embedded newlines.  (Perl's default).
  final val ONE_LINE: Int = 0x10

  // Make repetition operators default to non-greedy.
  final val NON_GREEDY: Int = 0x20

  // allow Perl extensions:
  //   non-capturing parens - (?: )
  //   non-greedy operators - *? +? ?? {}?
  //   flag edits - (?i) (?-i) (?i: )
  //     i - FoldCase
  //     m - !OneLine
  //     s - DotNL
  //     U - NonGreedy
  //   line ends: \A \z
  //   \Q and \E to disable/enable metacharacters
  //   (?P<name>expr) for named captures
  // \C (any byte) is not supported.
  final val PERL_X: Int = 0x40

  // Allow \p{Han}, \P{Han} for Unicode group and negation.
  final val UNICODE_GROUPS: Int = 0x80

  // Regexp END_TEXT was $, not \z.  Internal use only.
  final val WAS_DOLLAR: Int = 0x100

  final val MATCH_NL: Int = CLASS_NL | DOT_NL

  // As close to Perl as possible.
  final val PERL: Int = CLASS_NL | ONE_LINE | PERL_X | UNICODE_GROUPS

  // POSIX syntax.
  final val POSIX: Int = 0

  //// Anchors
  final val UNANCHORED: Int   = 0
  final val ANCHOR_START: Int = 1
  final val ANCHOR_BOTH: Int  = 2

  /**
   * Parses a regular expression and returns, if successful, an
   * {@code RE2} instance that can be used to match against text.
   *
   * <p>When matching against text, the regexp returns a match that
   * begins as early as possible in the input (leftmost), and among those
   * it chooses the one that a backtracking search would have found first.
   * This so-called leftmost-first matching is the same semantics
   * that Perl, Python, and other implementations use, although this
   * package implements it without the expense of backtracking.
   * For POSIX leftmost-longest matching, see {@link #compilePOSIX}.
   */
  def compile(expr: String): RE2 =
    compileImpl(expr, PERL, false)

  /**
   * {@code compilePOSIX} is like {@link #compile} but restricts the
   * regular expression to POSIX ERE (egrep) syntax and changes the
   * match semantics to leftmost-longest.
   *
   * <p>That is, when matching against text, the regexp returns a match that
   * begins as early as possible in the input (leftmost), and among those
   * it chooses a match that is as long as possible.
   * This so-called leftmost-longest matching is the same semantics
   * that early regular expression implementations used and that POSIX
   * specifies.
   *
   * <p>However, there can be multiple leftmost-longest matches, with different
   * submatch choices, and here this package diverges from POSIX.
   * Among the possible leftmost-longest matches, this package chooses
   * the one that a backtracking search would have found first, while POSIX
   * specifies that the match be chosen to maximize the length of the first
   * subexpression, then the second, and so on from left to right.
   * The POSIX rule is computationally prohibitive and not even well-defined.
   * See http://swtch.com/~rsc/regexp/regexp2.html#posix
   */
  def compilePOSIX(expr: String): RE2 =
    compileImpl(expr, POSIX, true)

  // Exposed to ExecTests.
  def compileImpl(expr: String, mode: Int, longest: Boolean): RE2 = {
    var re: Regexp     = Parser.parse(expr, mode)
    val maxCap: Int = re.maxCap() // (may shrink during simplify)
    re = Simplify.simplify(re)
    val prog: Prog          = Compiler.compileRegexp(re)
    val re2: RE2           = new RE2()
    re2.expr = expr
    re2.prog = prog
    re2.numSubexp = maxCap
    re2.cond = prog.startCond()
    re2.longest = longest
    val prefixBuilder: java.lang.StringBuilder = new java.lang.StringBuilder()
    re2.prefixComplete = prog.prefix(prefixBuilder)
    re2.prefix = prefixBuilder.toString()
    re2.prefixUTF8 = re2.prefix.getBytes("UTF-8")
    if (!re2.prefix.isEmpty()) {
      re2.prefixRune = re2.prefix.codePointAt(0)
    }
    re2
  }

  /**
   * Returns true iff textual regular expression {@code pattern}
   * matches string {@code s}.
   *
   * <p>More complicated queries need to use {@link #compile} and the
   * full {@code RE2} interface.
   */
  // This is visible for testing.
  def match_(pattern: String, s: CharSequence): Boolean =
    compile(pattern).match_1(s)

  /**
   * Returns a string that quotes all regular expression metacharacters
   * inside the argument text the returned string is a regular
   * expression matching the literal text.  For example,
   * {@code quoteMeta("[foo]").equals("\\[foo\\]")}.
   */
  def quoteMeta(s: String): String = {
    val b: java.lang.StringBuilder = new java.lang.StringBuilder(2 * s.length())
    // A char loop is correct because all metacharacters fit in one UTF-16 code.
    var i: Int   = 0
    var len: Int = s.length()
    while (i < len) {
      val c: Char = s.charAt(i)
      if ("\\.+*?()|[]{}^$".indexOf(c) >= 0) {
        b.append('\\')
      }
      b.append(c)
      i += 1
    }
    b.toString()
  }
}
