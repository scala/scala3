// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/parse.go

// TODO(adonovan):
// - Eliminate allocations (new int[], new Regexp[], new ArrayList) by
//   recycling old arrays on a freelist.

package java.util.regex

import java.util.ArrayList
import java.util.regex.Parser._
import java.util.regex.Regexp.{Op => ROP}

/**
 * A parser of regular expression patterns.
 *
 * The only public entry point is {@link #parse(String pattern, int flags)}.
 */
class Parser(wholeRegexp: String, _flags: Int) {
  // Flags control the behavior of the parser and record information about
  // regexp context.
  private var flags: Int = _flags // parse mode flags

  // Stack of parsed expressions.
  private val stack: Parser.Stack        = new Stack()
  private var free: Regexp = null
  private var numCap: Int       = 0 // number of capturing groups seen

  // Allocate a Regexp, from the free list if possible.
  private def newRegexp(op: ROP): Regexp = {
    var re: Regexp = free
    if (re != null && re.subs != null && re.subs.length > 0) {
      free = re.subs(0)
      re.reinit()
      re.op = op
    } else {
      re = new Regexp()
      re.op = op
    }
    re
  }

  private def reuse(re: Regexp): Unit = {
    if (re.subs != null && re.subs.length > 0) {
      re.subs(0) = free
    }
    free = re
  }

  // Parse stack manipulation.

  private def pop(): Regexp =
    stack.remove(stack.size() - 1)

  private def popToPseudo(): Array[Regexp] = {
    var n: Int = stack.size()
    var i: Int = n
    while (i > 0 && !ROP.isPseudo(stack.get(i - 1).op)) {
      i -= 1
    }
    val r: Array[Regexp] = stack.subList(i, n).toArray(new Array[Regexp](n - i))
    stack.removeRange(i, n)
    r
  }

  // push pushes the regexp re onto the parse stack and returns the regexp.
  // Returns null for a CHAR_CLASS that can be merged with the top-of-stack.
  private def push(re: Regexp): Regexp = {
    if (re.op == ROP.CHAR_CLASS &&
        re.runes.length == 2 &&
        re.runes(0) == re.runes(1)) {
      // Collapse range [x-x] -> single rune x.
      if (maybeConcat(re.runes(0), flags & ~RE2.FOLD_CASE)) {
        return null
      }
      re.op = ROP.LITERAL
      re.runes = Array[Int](re.runes(0))
      re.flags = flags & ~RE2.FOLD_CASE
    } else if ((re.op == ROP.CHAR_CLASS &&
               re.runes.length == 4 &&
               re.runes(0) == re.runes(1) &&
               re.runes(2) == re.runes(3) &&
               Unicode.simpleFold(re.runes(0)) == re.runes(2) &&
               Unicode.simpleFold(re.runes(2)) == re.runes(0)) ||
               (re.op == ROP.CHAR_CLASS &&
               re.runes.length == 2 &&
               re.runes(0) + 1 == re.runes(1) &&
               Unicode.simpleFold(re.runes(0)) == re.runes(1) &&
               Unicode.simpleFold(re.runes(1)) == re.runes(0))) {
      // Case-insensitive rune like [Aa] or [Δδ].
      if (maybeConcat(re.runes(0), flags | RE2.FOLD_CASE)) {
        return null
      }

      // Rewrite as (case-insensitive) literal.
      re.op = ROP.LITERAL
      re.runes = Array[Int](re.runes(0))
      re.flags = flags | RE2.FOLD_CASE
    } else {
      // Incremental concatenation.
      maybeConcat(-1, 0)
    }

    stack.add(re)
    re
  }

  // maybeConcat implements incremental concatenation
  // of literal runes into string nodes.  The parser calls this
  // before each push, so only the top fragment of the stack
  // might need processing.  Since this is called before a push,
  // the topmost literal is no longer subject to operators like *
  // (Otherwise ab* would turn into (ab)*.)
  // If (r >= 0 and there's a node left over, maybeConcat uses it
  // to push r with the given flags.
  // maybeConcat reports whether r was pushed.
  private def maybeConcat(r: Int, flags: Int): Boolean = {
    val n: Int = stack.size()
    if (n < 2) {
      return false
    }
    val re1: Regexp = stack.get(n - 1)
    val re2: Regexp = stack.get(n - 2)
    if (re1.op != ROP.LITERAL ||
        re2.op != ROP.LITERAL ||
        (re1.flags & RE2.FOLD_CASE) != (re2.flags & RE2.FOLD_CASE)) {
      return false
    }

    // Push re1 into re2.
    re2.runes = concatRunes(re2.runes, re1.runes)

    // Reuse re1 if possible.
    if (r >= 0) {
      re1.runes = Array[Int](r)
      re1.flags = flags
      return true
    }

    pop()
    reuse(re1)
    false // did not push r
  }

  // newLiteral returns a new LITERAL Regexp with the given flags
  private def newLiteral(_r: Int, flags: Int): Regexp = {
    var r: Int  = _r
    val re: Regexp = newRegexp(ROP.LITERAL)
    re.flags = flags
    if ((flags & RE2.FOLD_CASE) != 0) {
      r = minFoldRune(r)
    }
    re.runes = new Array[Int](r)
    re
  }

  // literal pushes a literal regexp for the rune r on the stack
  // and returns that regexp.
  private def literal(r: Int): Unit =
    push(newLiteral(r, flags))

  // op pushes a regexp with the given op onto the stack
  // and returns that regexp.
  private def op(op: ROP): Regexp = {
    val re: Regexp = newRegexp(op)
    re.flags = flags
    push(re)
  }

  // repeat replaces the top stack element with itself repeated according to
  // op, min, max.  beforePos is the start position of the repetition operator.
  // Pre: t is positioned after the initial repetition operator.
  // Post: t advances past an optional perl-mode '?', or stays put.
  //       Or, it fails with PatternSyntaxException.
  private def repeat(op: ROP,
                     min: Int,
                     max: Int,
                     beforePos: Int,
                     t: StringIterator,
                     lastRepeatPos: Int): Unit = {
    var flags: Int = this.flags
    if ((flags & RE2.PERL_X) != 0) {
      if (t.more() && t.lookingAt_c('?')) {
        t.skip(1) // '?'
        flags ^= RE2.NON_GREEDY
      }
      if (lastRepeatPos != -1) {
        // In Perl it is not allowed to stack repetition operators:
        // a** is a syntax error, not a doubled star, and a++ means
        // something else entirely, which we don't support!
        throw new PatternSyntaxException(ERR_INVALID_REPEAT_OP,
                                         t.from(lastRepeatPos))
      }
    }
    val n: Int = stack.size()
    if (n == 0) {
      throw new PatternSyntaxException(ERR_MISSING_REPEAT_ARGUMENT,
                                       t.from(beforePos))
    }
    val sub: Regexp = stack.get(n - 1)
    if (ROP.isPseudo(sub.op)) {
      throw new PatternSyntaxException(ERR_MISSING_REPEAT_ARGUMENT,
                                       t.from(beforePos))
    }
    val re: Regexp = newRegexp(op)
    re.min = min
    re.max = max
    re.flags = flags
    re.subs = Array[Regexp](sub)
    stack.set(n - 1, re)
  }

  // concat replaces the top of the stack (above the topmost '|' or '(') with
  // its concatenation.
  private def concat(): Regexp = {
    maybeConcat(-1, 0)

    // Scan down to find pseudo-operator | or (.
    val subs: Array[Regexp] = popToPseudo()

    // Empty concatenation is special case.
    if (subs.length == 0) {
      return push(newRegexp(ROP.EMPTY_MATCH))
    }

    push(collapse(subs, ROP.CONCAT))
  }

  // alternate replaces the top of the stack (above the topmost '(') with its
  // alternation.
  private def alternate(): Regexp = {
    // Scan down to find pseudo-operator (.
    // There are no | above (.
    val subs: Array[Regexp] = popToPseudo()

    // Make sure top class is clean.
    // All the others already are (see swapVerticalBar).
    if (subs.length > 0) {
      cleanAlt(subs(subs.length - 1))
    }

    // Empty alternate is special case
    // (shouldn't happen but easy to handle).
    if (subs.length == 0) {
      return push(newRegexp(ROP.NO_MATCH))
    }

    push(collapse(subs, ROP.ALTERNATE))
  }

  // cleanAlt cleans re for eventual inclusion in an alternation.
  private def cleanAlt(re: Regexp): Unit = {
    re.op match {
      case ROP.CHAR_CLASS =>
        re.runes = new CharClass(re.runes).cleanClass().toArray()
        if (re.runes.length == 2 &&
            re.runes(0) == 0 &&
            re.runes(1) == Unicode.MAX_RUNE) {
          re.runes = null
          re.op = ROP.ANY_CHAR
          return
        }
        if (re.runes.length == 4 &&
            re.runes(0) == 0 &&
            re.runes(1) == '\n' - 1 &&
            re.runes(2) == '\n' + 1 &&
            re.runes(3) == Unicode.MAX_RUNE) {
          re.runes = null
          re.op = ROP.ANY_CHAR_NOT_NL
          return
        }
    }
  }

  // collapse returns the result of applying op to subs[start:end].
  // If (sub contains op nodes, they all get hoisted up
  // so that there is never a concat of a concat or an
  // alternate of an alternate.
  private def collapse(subs: Array[Regexp], op: ROP): Regexp = {
    if (subs.length == 1) {
      return subs(0)
    }
    // Concatenate subs iff op is same.
    // Compute length in first pass.
    var len: Int = 0
    var i: Int   = 0
    while (i < subs.length) {
      val sub: Regexp = subs(i)
      len += (if (sub.op == op) sub.subs.length else 1)
      i += 1
    }
    val newsubs: Array[Regexp] = new Array[Regexp](len)
    i = 0
    var j: Int = 0
    while (j < subs.length) {
      val sub: Regexp = subs(j)
      if (sub.op == op) {
        System.arraycopy(sub.subs, 0, newsubs, i, sub.subs.length)
        i += sub.subs.length
        reuse(sub)
      } else {
        newsubs(i) = sub
        i += 1
      }
      j += 1
    }
    var re: Regexp = newRegexp(op)
    re.subs = newsubs

    if (op == ROP.ALTERNATE) {
      re.subs = factor(re.subs, re.flags)
      if (re.subs.length == 1) {
        val old: Regexp = re
        re = re.subs(0)
        reuse(old)
      }
    }
    re
  }

  // factor factors common prefixes from the alternation list sub.  It
  // returns a replacement list that reuses the same storage and frees
  // (passes to p.reuse) any removed *Regexps.
  //
  // For example,
  //     ABC|ABD|AEF|BCX|BCY
  // simplifies by literal prefix extraction to
  //     A(B(C|D)|EF)|BC(X|Y)
  // which simplifies by character class introduction to
  //     A(B[CD]|EF)|BC[XY]
  //
  private def factor(array: Array[Regexp], flags: Int): Array[Regexp] = {
    if (array.length < 2) {
      return array
    }

    // The following code is subtle, because it's a literal Java
    // translation of code that makes clever use of Go "slices".
    // A slice is a triple (array, offset, length), and the Go
    // implementation uses two slices, |sub| and |out| backed by the
    // same array.  In Java, we have to be explicit about all of these
    // variables, so:
    //
    // Go    Java
    // sub   (array, s, lensub)
    // out   (array, 0, lenout)   // (always a prefix of |array|)
    //
    // In the comments we'll use the logical notation of go slices, e.g. sub[i]
    // even though the Java code will read array[s + i].

    var s: Int      = 0 // offset of first |sub| within array.
    var lensub: Int = array.length // = len(sub)
    var lenout: Int = 0 // = len(out)

    // Round 1: Factor out common literal prefixes.
    // Note: (str, strlen) and (istr, istrlen) are like Go slices
    // onto a prefix of some Regexp's runes array (hence offset=0).
    var str: Array[Int] = null
    var strlen: Int          = 0
    var strflags: Int        = 0
    var start: Int           = 0
    var i: Int               = 0
    while (i <= lensub) {
      // Invariant: the Regexps that were in sub[0:start] have been
      // used or marked for reuse, and the slice space has been reused
      // for out (len <= start).
      //
      // Invariant: sub[start:i] consists of regexps that all begin
      // with str as modified by strflags.
      var istr: Array[Int] = null
      var istrlen: Int          = 0
      var iflags: Int           = 0
      var continue: Boolean         = false
      if (i < lensub) {
        // NB, we inlined Go's leadingString() since Java has no pair return.
        var re: Regexp = array(s + i)
        if (re.op == ROP.CONCAT && re.subs.length > 0) {
          re = re.subs(0)
        }
        if (re.op == ROP.LITERAL) {
          istr = re.runes
          istrlen = re.runes.length
          iflags = re.flags & RE2.FOLD_CASE
        }
        // istr is the leading literal string that re begins with.
        // The string refers to storage in re or its children.

        if (iflags == strflags) {
          var same: Int = 0
          while (same < strlen &&
                 same < istrlen &&
                 str(same) == istr(same)) {
            same += 1
          }
          if (same > 0) {
            // Matches at least one rune in current range.
            // Keep going around.
            strlen = same
            continue = true
          }
        }
      }

      if (!continue) {
        // Found end of a run with common leading literal string:
        // sub[start:i] all begin with str[0:strlen], but sub[i]
        // does not even begin with str[0].
        //
        // Factor out common string and append factored expression to out.
        if (i == start) {
          // Nothing to do - run of length 0.
        } else if (i == start + 1) {
          // Just one: don't bother factoring.
          array(lenout) = array(s + start)
          lenout += 1
        } else {
          // Construct factored form: prefix(suffix1|suffix2|...)
          val prefix: Regexp = newRegexp(ROP.LITERAL)
          prefix.flags = strflags
          prefix.runes = Utils.subarray_i(str, 0, strlen)

          var j: Int = start
          while (j < i) {
            array(s + j) = removeLeadingString(array(s + j), strlen)
            j += 1
          }
          // Recurse.
          val suffix: Regexp =
            collapse(subarray(array, s + start, s + i), ROP.ALTERNATE)
          val re: Regexp = newRegexp(ROP.CONCAT)
          re.subs = Array[Regexp](prefix, suffix)
          array(lenout) = re
          lenout += 1
        }

        // Prepare for next iteration.
        start = i
        str = istr
        strlen = istrlen
        strflags = iflags
      }

      i += 1
    }
    // In Go: sub = out
    lensub = lenout
    s = 0

    // Round 2: Factor out common complex prefixes,
    // just the first piece of each concatenation,
    // whatever it is.  This is good enough a lot of the time.
    start = 0
    lenout = 0
    var first: Regexp = null
    i = 0
    while (i <= lensub) {
      // Invariant: the Regexps that were in sub[0:start] have been
      // used or marked for reuse, and the slice space has been reused
      // for out (lenout <= start).
      //
      // Invariant: sub[start:i] consists of regexps that all begin with
      // ifirst.
      var ifirst: Regexp = null
      var continue: Boolean       = false
      if (i < lensub) {
        ifirst = leadingRegexp(array(s + i))
        if (first != null && first.equals(ifirst)) {
          continue = true
        }
      }

      if (!continue) {
        // Found end of a run with common leading regexp:
        // sub[start:i] all begin with first but sub[i] does not.
        //
        // Factor out common regexp and append factored expression to out.
        if (i == start) {
          // Nothing to do - run of length 0.
        } else if (i == start + 1) {
          // Just one: don't bother factoring.
          array(lenout) = array(s + start)
          lenout += 1
        } else {
          // Construct factored form: prefix(suffix1|suffix2|...)
          val prefix: Regexp = first
          var j: Int      = start
          while (j < i) {
            val reuse: Boolean = j != start // prefix came from sub[start]
            array(s + j) = removeLeadingRegexp(array(s + j), reuse)
            j += 1
          }
          // recurse
          val suffix: Regexp =
            collapse(subarray(array, s + start, s + i), ROP.ALTERNATE)
          val re: Regexp = newRegexp(ROP.CONCAT)
          re.subs = Array[Regexp](prefix, suffix)
          array(lenout) = re
          lenout += 1
        }

        // Prepare for next iteration.
        start = i
        first = ifirst
      }

      i += 1
    }
    // In Go: sub = out
    lensub = lenout
    s = 0

    // Round 3: Collapse runs of single literals into character classes.
    start = 0
    lenout = 0
    i = 0
    while (i <= lensub) {
      // Invariant: the Regexps that were in sub[0:start] have been
      // used or marked for reuse, and the slice space has been reused
      // for out (lenout <= start).
      //
      // Invariant: sub[start:i] consists of regexps that are either
      // literal runes or character classes.
      var continue: Boolean = false
      if (i < lensub && isCharClass(array(s + i))) {
        continue = true
      }

      if (!continue) {
        // sub[i] is not a char or char class
        // emit char class for sub[start:i]...
        if (i == start) {
          // Nothing to do - run of length 0.
        } else if (i == start + 1) {
          array(lenout) = array(s + start)
          lenout += 1
        } else {
          // Make new char class.
          // Start with most complex regexp in sub[start].
          var max: Int = start
          var j: Int   = start + 1
          while (j < i) {
            val subMax: Regexp = array(s + max)
            val subJ: Regexp   = array(s + j)
            if (subMax.op < subJ.op ||
                subMax.op == subJ.op && subMax.runes.length < subJ.runes.length) {
              max = j
            }
            j += 1
          }
          // swap sub[start], sub[max].
          val tmp: Regexp = array(s + start)
          array(s + start) = array(s + max)
          array(s + max) = tmp

          j = start + 1
          while (j < i) {
            mergeCharClass(array(s + start), array(s + j))
            reuse(array(s + j))
            j += 1
          }
          cleanAlt(array(s + start))
          array(lenout) = array(s + start)
          lenout += 1
        }

        // ... and then emit sub[i].
        if (i < lensub) {
          array(lenout) = array(s + i)
          lenout += 1
        }
        start = i + 1
      }

      i += 1
    }
    // In Go: sub = out
    lensub = lenout
    s = 0

    // Round 4: Collapse runs of empty matches into a single empty match.
    start = 0
    lenout = 0
    i = 0
    while (i < lensub) {
      var continue: Boolean = false
      if (i + 1 < lensub &&
          array(s + i).op == ROP.EMPTY_MATCH &&
          array(s + i + 1).op == ROP.EMPTY_MATCH) {
        continue = true
      }
      if (!continue) {
        array(lenout) = array(s + i)
        lenout += 1
      }
      i += 1
    }
    // In Go: sub = out
    lensub = lenout
    s = 0

    subarray(array, s, lensub)
  }

  // removeLeadingString removes the first n leading runes
  // from the beginning of re.  It returns the replacement for re.
  private def removeLeadingString(_re: Regexp, n: Int): Regexp = {
    var re: Regexp = _re
    if (re.op == ROP.CONCAT && re.subs.length > 0) {
      // Removing a leading string in a concatenation
      // might simplify the concatenation.
      val sub: Regexp = removeLeadingString(re.subs(0), n)
      re.subs(0) = sub
      if (sub.op == ROP.EMPTY_MATCH) {
        reuse(sub)
        re.subs.length match {
          case 0 | 1 =>
            // Impossible but handle.
            re.op = ROP.EMPTY_MATCH
            re.subs = null
          case 2 =>
            val old: Regexp = re
            re = re.subs(1)
            reuse(old)
          case _ =>
            re.subs = subarray(re.subs, 1, re.subs.length)
        }
      }
      return re
    }

    if (re.op == ROP.LITERAL) {
      re.runes = Utils.subarray_i(re.runes, n, re.runes.length)
      if (re.runes.length == 0) {
        re.op = ROP.EMPTY_MATCH
      }
    }
    re
  }

  // removeLeadingRegexp removes the leading regexp in re.
  // It returns the replacement for re.
  // If reuse is true, it passes the removed regexp (if no longer needed) to
  // reuse.
  private def removeLeadingRegexp(_re: Regexp, reuse: Boolean): Regexp = {
    var re: Regexp = _re
    if (re.op == ROP.CONCAT && re.subs.length > 0) {
      if (reuse) {
        this.reuse(re.subs(0))
      }
      re.subs = subarray(re.subs, 1, re.subs.length)
      re.subs.length match {
        case 0 =>
          re.op = ROP.EMPTY_MATCH
          re.subs = Regexp.EMPTY_SUBS
        case 1 =>
          val old: Regexp = re
          re = re.subs(0)
          this.reuse(old)
      }
      return re
    }
    if (reuse) {
      this.reuse(re)
    }
    return newRegexp(ROP.EMPTY_MATCH)
  }

  // Parsing.

  private def parseInternal(): Regexp = {
    if ((flags & RE2.LITERAL) != 0) {
      // Trivial parser for literal string.
      return literalRegexp(wholeRegexp, flags)
    }

    // Otherwise, must do real work.
    var lastRepeatPos: Int = -1
    var min: Int           = -1
    var max: Int           = -1
    var t: Parser.StringIterator             = new StringIterator(wholeRegexp)
    while (t.more()) {
      var repeatPos: Int = -1
      t.peek() match {
        case '(' =>
          if ((flags & RE2.PERL_X) != 0 && t.lookingAt_s("(?")) {
            // Flag changes and non-capturing groups.
            parsePerlFlags(t)
          } else {
            numCap += 1
            op(ROP.LEFT_PAREN).cap = numCap
            t.skip(1) // '('
          }

        case '|' =>
          parseVerticalBar()
          t.skip(1) // '|'

        case ')' =>
          parseRightParen()
          t.skip(1) // ')'

        case '^' =>
          if ((flags & RE2.ONE_LINE) != 0) {
            op(ROP.BEGIN_TEXT)
          } else {
            op(ROP.BEGIN_LINE)
          }
          t.skip(1) // '^'

        case '$' =>
          if ((flags & RE2.ONE_LINE) != 0) {
            op(ROP.END_TEXT).flags |= RE2.WAS_DOLLAR
          } else {
            op(ROP.END_LINE)
          }
          t.skip(1) // '$'

        case '.' =>
          if ((flags & RE2.DOT_NL) != 0) {
            op(ROP.ANY_CHAR)
          } else {
            op(ROP.ANY_CHAR_NOT_NL)
          }
          t.skip(1) // '.'

        case '[' =>
          parseClass(t)

        case '*' | '+' | '?' =>
          repeatPos = t.pos()
          val op: Int =
            t.pop() match {
              case '*' => ROP.STAR
              case '+' => ROP.PLUS
              case '?' => ROP.QUEST
            }
          repeat(op, min, max, repeatPos, t, lastRepeatPos)
        // (min and max are now dead.)

        case '{' =>
          repeatPos = t.pos()
          val minMax: Int = parseRepeat(t)
          if (minMax < 0) {
            // If the repeat cannot be parsed, { is a literal.
            t.rewindTo(repeatPos)
            literal(t.pop()) // '{'
          } else {
            min = minMax >> 16
            max = minMax.&(0xffff).toShort // sign extend
            repeat(ROP.REPEAT, min, max, repeatPos, t, lastRepeatPos)
          }

        case '\\' =>
          var breakBigswitch: Boolean = false
          val savedPos: Int       = t.pos()
          t.skip(1) // '\\'
          if ((flags & RE2.PERL_X) != 0 && t.more()) {
            val c: Int = t.pop()
            c match {
              case 'A' =>
                op(ROP.BEGIN_TEXT)
                breakBigswitch = true
              case 'b' =>
                op(ROP.WORD_BOUNDARY)
                breakBigswitch = true
              case 'B' =>
                op(ROP.NO_WORD_BOUNDARY)
                breakBigswitch = true
              case 'C' =>
                // any byte not supported
                throw new PatternSyntaxException(ERR_INVALID_ESCAPE, "\\C")
              case 'Q' =>
                // \Q ... \E: the ... is always literals
                var lit: String = t.rest()
                val i: Int   = lit.indexOf("\\E")
                if (i >= 0) {
                  lit = lit.substring(0, i)
                }
                t.skipString(lit)
                t.skipString("\\E")
                push(literalRegexp(lit, flags))
                breakBigswitch = true
              case 'z' =>
                op(ROP.END_TEXT)
                breakBigswitch = true
              case _ =>
                t.rewindTo(savedPos)
            }
          }

          if (!breakBigswitch) {
            val re: Regexp = newRegexp(ROP.CHAR_CLASS)
            re.flags = flags

            // Look for Unicode character group like \p{Han}
            if (t.lookingAt_s("\\p") || t.lookingAt_s("\\P")) {
              val cc: CharClass = new CharClass(Utils.EMPTY_INTS)
              if (parseUnicodeClass(t, cc)) {
                re.runes = cc.toArray()
                push(re)
                breakBigswitch = true
              }
            }

            if (!breakBigswitch) {
              // Perl character class escape.
              val cc: CharClass = new CharClass(Utils.EMPTY_INTS)
              if (parsePerlClassEscape(t, cc)) {
                re.runes = cc.toArray()
                push(re)
                breakBigswitch = true
              }
            }

            if (!breakBigswitch) {
              t.rewindTo(savedPos)
              this.reuse(re)

              // Ordinary single-character escape.
              literal(parseEscape(t))
            }
          }

        case _ =>
          literal(t.pop())
      }
      lastRepeatPos = repeatPos
    }

    concat()
    if (swapVerticalBar()) {
      pop() // pop vertical bar
    }
    alternate()

    val n: Int = stack.size()
    if (n != 1) {
      throw new PatternSyntaxException(ERR_MISSING_PAREN, wholeRegexp)
    }
    stack.get(0)
  }

  // parsePerlFlags parses a Perl flag setting or non-capturing group or both,
  // like (?i) or (?: or (?i:.
  // Pre: t at "(?".  Post: t after ")".
  // Sets numCap.
  private def parsePerlFlags(t: StringIterator): Unit = {
    val startPos: Int = t.pos()

    // Check for named captures, first introduced in Python's regexp library.
    // As usual, there are three slightly different syntaxes:
    //
    //   (?P<name>expr)   the original, introduced by Python
    //   (?<name>expr)    the .NET alteration, adopted by Perl 5.10
    //   (?'name'expr)    another .NET alteration, adopted by Perl 5.10
    //
    // Perl 5.10 gave in and implemented the Python version too,
    // but they claim that the last two are the preferred forms.
    // PCRE and languages based on it (specifically, PHP and Ruby)
    // support all three as well.  EcmaScript 4 uses only the Python form.
    //
    // In both the open source world (via Code Search) and the
    // Google source tree, (?P<expr>name) is the dominant form,
    // so that's the one we implement.  One is enough.
    val s: String = t.rest()
    if (s.startsWith("(?P<")) {
      // Pull out name.
      val end: Int = s.indexOf('>')
      if (end < 0) {
        throw new PatternSyntaxException(ERR_INVALID_NAMED_CAPTURE, s)
      }
      val name: String = s.substring(4, end) // "name"
      t.skipString(name)
      t.skip(5) // "(?P<>"
      if (!isValidCaptureName(name)) {
        throw new PatternSyntaxException(ERR_INVALID_NAMED_CAPTURE,
                                         s.substring(0, end)) // "(?P<name>"
      }
      // Like ordinary capture, but named.
      val re: Regexp = op(ROP.LEFT_PAREN)
      numCap += 1
      re.cap = numCap
      re.name = name
      return
    }

    // Non-capturing group.  Might also twiddle Perl flags.
    t.skip(2) // "(?"
    var flags: Int   = this.flags
    var sign: Int    = +1
    var sawFlag: Boolean = false

    var breakLoop: Boolean = false
    while (t.more() && !breakLoop) {
      val c: Int = t.pop()
      c match {
        // Flags.
        case 'i' =>
          flags |= RE2.FOLD_CASE
          sawFlag = true
        case 'm' =>
          flags &= ~RE2.ONE_LINE
          sawFlag = true
        case 's' =>
          flags |= RE2.DOT_NL
          sawFlag = true
        case 'U' =>
          flags |= RE2.NON_GREEDY
          sawFlag = true

        // Switch to negation.
        case '-' =>
          if (sign < 0) {
            breakLoop = true
          } else {
            sign = -1
            // Invert flags so that | above turn into &~ and vice versa.
            // We'll invert flags again before using it below.
            flags = ~flags
            sawFlag = false
          }

        // End of flags, starting group or not.
        case ':' | ')' =>
          if (sign < 0) {
            if (!sawFlag) {
              breakLoop = true
            }
            if (!breakLoop) {
              flags = ~flags
            }
          }
          if (!breakLoop) {
            if (c == ':') {
              // Open new group
              op(ROP.LEFT_PAREN)
            }
            this.flags = flags
            return
          }

        case _ =>
          breakLoop = true
      }
    }

    throw new PatternSyntaxException(ERR_INVALID_PERL_OP, t.from(startPos))
  }

  // parseVerticalBar handles a | in the input.
  private def parseVerticalBar(): Unit = {
    concat()

    // The concatenation we just parsed is on top of the stack.
    // If it sits above an opVerticalBar, swap it below
    // (things below an opVerticalBar become an alternation).
    // Otherwise, push a new vertical bar.
    if (!swapVerticalBar()) {
      op(ROP.VERTICAL_BAR)
    }
  }

  // If the top of the stack is an element followed by an opVerticalBar
  // swapVerticalBar swaps the two and returns true.
  // Otherwise it returns false.
  private def swapVerticalBar(): Boolean = {
    // If above and below vertical bar are literal or char class,
    // can merge into a single char class.
    val n: Int = stack.size()
    if (n >= 3 &&
        stack.get(n - 2).op == ROP.VERTICAL_BAR &&
        isCharClass(stack.get(n - 1)) &&
        isCharClass(stack.get(n - 3))) {
      var re1: Regexp = stack.get(n - 1)
      var re3: Regexp = stack.get(n - 3)
      // Make re3 the more complex of the two.
      if (re1.op > re3.op) {
        val tmp: Regexp = re3
        re3 = re1
        re1 = tmp
        stack.set(n - 3, re3)
      }
      mergeCharClass(re3, re1)
      this.reuse(re1)
      pop()
      return true
    }

    if (n >= 2) {
      val re1: Regexp = stack.get(n - 1)
      val re2: Regexp = stack.get(n - 2)
      if (re2.op == ROP.VERTICAL_BAR) {
        if (n >= 3) {
          // Now out of reach.
          // Clean opportunistically.
          cleanAlt(stack.get(n - 3))
        }
        stack.set(n - 2, re1)
        stack.set(n - 1, re2)
        return true
      }
    }
    return false
  }

  // parseRightParen handles a ')' in the input.
  private def parseRightParen(): Unit = {
    concat()
    if (swapVerticalBar()) {
      pop() // pop vertical bar
    }
    alternate()

    val n: Int = stack.size()
    if (n < 2) {
      throw new PatternSyntaxException(ERR_INTERNAL_ERROR, "stack underflow")
    }
    val re1: Regexp = pop()
    val re2: Regexp = pop()
    if (re2.op != ROP.LEFT_PAREN) {
      throw new PatternSyntaxException(ERR_MISSING_PAREN, wholeRegexp)
    }
    // Restore flags at time of paren.
    this.flags = re2.flags
    if (re2.cap == 0) {
      // Just for grouping.
      push(re1)
    } else {
      re2.op = ROP.CAPTURE
      re2.subs = Array[Regexp](re1)
      push(re2)
    }
  }

  // parsePerlClassEscape parses a leading Perl character class escape like \d
  // from the beginning of |t|.  If one is present, it appends the characters
  // to cc and returns true.  The iterator is advanced past the escape
  // on success, undefined on failure, in which case false is returned.
  private def parsePerlClassEscape(t: StringIterator, cc: CharClass): Boolean = {
    val beforePos: Int = t.pos()
    if ((flags & RE2.PERL_X) == 0 ||
        !t.more() || t.pop() != '\\' || // consume '\\'
        !t.more()) {
      return false
    }
    t.pop() // e.g. advance past 'd' in "\\d"
    val g: CharGroup = CharGroup.PERL_GROUPS.get(t.from(beforePos))
    if (g == null) {
      return false
    }
    cc.appendGroup(g, (flags & RE2.FOLD_CASE) != 0)
    return true
  }

  // parseNamedClass parses a leading POSIX named character class like
  // [:alnum:] from the beginning of t.  If one is present, it appends the
  // characters to cc, advances the iterator, and returns true.
  // Pre: t at "[:".  Post: t after ":]".
  // On failure (no class of than name), throws PatternSyntaxException.
  // On misparse, returns false t.pos() is undefined.
  private def parseNamedClass(t: StringIterator, cc: CharClass): Boolean = {
    // (Go precondition check deleted.)
    val cls: String = t.rest()
    val i: Int   = cls.indexOf(":]")
    if (i < 0) {
      return false
    }
    val name: String = cls.substring(0, i + 2) // "[:alnum:]"
    t.skipString(name)
    val g: CharGroup = CharGroup.POSIX_GROUPS.get(name)
    if (g == null) {
      throw new PatternSyntaxException(ERR_INVALID_CHAR_RANGE, name)
    }
    cc.appendGroup(g, (flags & RE2.FOLD_CASE) != 0)
    true
  }

  // parseUnicodeClass() parses a leading Unicode character class like \p{Han}
  // from the beginning of t.  If one is present, it appends the characters to
  // to |cc|, advances |t| and returns true.
  //
  // Returns false if such a pattern is not present or UNICODE_GROUPS
  // flag is not enabled |t.pos()| is not advanced in this case.
  // Indicates error by throwing PatternSyntaxException.
  private def parseUnicodeClass(t: StringIterator, cc: CharClass): Boolean = {
    val startPos: Int = t.pos()
    if ((flags & RE2.UNICODE_GROUPS) == 0 ||
        !t.lookingAt_s("\\p") && !t.lookingAt_s("\\P")) {
      return false
    }
    t.skip(1) // '\\'
    // Committed to parse or throw exception.
    var sign: Int = +1
    var c: Int    = t.pop() // 'p' or 'P'
    if (c == 'P') {
      sign = -1
    }
    c = t.pop()
    var name: String = ""
    if (c != '{') {
      // Single-letter name.
      name = Utils.runeToString(c)
    } else {
      // Name is in braces.
      val rest: String = t.rest()
      val end: Int  = rest.indexOf('}')
      if (end < 0) {
        t.rewindTo(startPos)
        throw new PatternSyntaxException(ERR_INVALID_CHAR_RANGE, t.rest())
      }
      name = rest.substring(0, end) // e.g. "Han"
      t.skipString(name)
      t.skip(1) // '}'
      // Don't use skip(end) because it assumes UTF-16 coding, and
      // StringIterator doesn't guarantee that.
    }

    // Group can have leading negation too.
    //  \p{^Han} == \P{Han}, \P{^Han} == \p{Han}.
    if (!name.isEmpty() && name.charAt(0) == '^') {
      sign = -sign
      name = name.substring(1)
    }

    val pair: Parser.Pair[Array[Array[Int]], Array[Array[Int]]] = unicodeTable(name)
    if (pair == null) {
      throw new PatternSyntaxException(ERR_INVALID_CHAR_RANGE,
                                       t.from(startPos))
    }
    val tab: Array[Array[Int]]  = pair.first
    val fold: Array[Array[Int]] = pair.second // fold-equivalent table

    // Variation of CharClass.appendGroup() for tables.
    if ((flags & RE2.FOLD_CASE) == 0 || fold == null) {
      cc.appendTableWithSign(tab, sign)
    } else {
      // Merge and clean tab and fold in a temporary buffer.
      // This is necessary for the negative case and just tidy
      // for the positive case.
      val tmp: Array[Int] = new CharClass(Utils.EMPTY_INTS)
        .appendTable(tab)
        .appendTable(fold)
        .cleanClass()
        .toArray()
      cc.appendClassWithSign(tmp, sign)
    }
    return true
  }

  // parseClass parses a character class and pushes it onto the parse stack.
  //
  // NOTES:
  // Pre: at '[' Post: after ']'.
  // Mutates stack.  Advances iterator.  May throw.
  private def parseClass(t: StringIterator): Unit = {
    var startPos: Int = t.pos()
    t.skip(1) // '['
    val re: Regexp = newRegexp(ROP.CHAR_CLASS)
    re.flags = flags
    val cc: CharClass = new CharClass(Utils.EMPTY_INTS)

    var sign: Int = +1
    if (t.more() && t.lookingAt_c('^')) {
      sign = -1
      t.skip(1) // '^'

      // If character class does not match \n, add it here,
      // so that negation later will do the right thing.
      if ((flags & RE2.CLASS_NL) == 0) {
        cc.appendRange('\n', '\n')
      }
    }

    var first: Boolean = true // ']' and '-' are okay as first char in class
    while (!t.more() || t.peek() != ']' || first) {
      var continue: Boolean = false

      // POSIX: - is only okay unescaped as first or last in class.
      // Perl: - is okay anywhere.
      if (t.more() && t.lookingAt_c('-') &&
          (flags & RE2.PERL_X) == 0 &&
          !first) {
        val s: String = t.rest()
        if (s.equals("-") || !s.startsWith("-]")) {
          t.rewindTo(startPos)
          throw new PatternSyntaxException(ERR_INVALID_CHAR_RANGE, t.rest())
        }
      }
      first = false

      val beforePos: Int = t.pos()

      // Look for POSIX [:alnum:] etc.
      if (t.lookingAt_s("[:")) {
        if (parseNamedClass(t, cc)) {
          continue = true
        }
        if (!continue) {
          t.rewindTo(beforePos)
        }
      }

      if (!continue) {
        // Look for Unicode character group like \p{Han}.
        if (parseUnicodeClass(t, cc)) {
          continue = true
        } else {
          // Look for Perl character class symbols (extension).
          if (parsePerlClassEscape(t, cc)) {
            continue = true
          } else {
            t.rewindTo(beforePos)

            // Single character or simple range.
            var lo: Int = parseClassChar(t, startPos)
            var hi: Int = lo
            if (t.more() && t.lookingAt_c('-')) {
              t.skip(1) // '-'
              if (t.more() && t.lookingAt_c(']')) {
                // [a-] means (a|-) so check for final ].
                t.skip(-1)
              } else {
                hi = parseClassChar(t, startPos)
                if (hi < lo) {
                  throw new PatternSyntaxException(ERR_INVALID_CHAR_RANGE,
                                                   t.from(beforePos))
                }
              }
            }
            if ((flags & RE2.FOLD_CASE) == 0) {
              cc.appendRange(lo, hi)
            } else {
              cc.appendFoldedRange(lo, hi)
            }
          }
        }
      }
    }
    t.skip(1) // ']'

    cc.cleanClass()
    if (sign < 0) {
      cc.negateClass()
    }
    re.runes = cc.toArray()
    push(re)
  }
}

object Parser {

  // Unexpected error
  private final val ERR_INTERNAL_ERROR: String =
    "regexp/syntax: internal error"

  // Parse errors
  private final val ERR_INVALID_CHAR_CLASS: String =
    "invalid character class"
  private final val ERR_INVALID_CHAR_RANGE: String =
    "invalid character class range"
  private final val ERR_INVALID_ESCAPE: String =
    "invalid escape sequence"
  private final val ERR_INVALID_NAMED_CAPTURE: String =
    "invalid named capture"
  private final val ERR_INVALID_PERL_OP: String =
    "invalid or unsupported Perl syntax"
  private final val ERR_INVALID_REPEAT_OP: String =
    "invalid nested repetition operator"
  private final val ERR_INVALID_REPEAT_SIZE: String =
    "invalid repeat count"
  private final val ERR_MISSING_BRACKET: String =
    "missing closing ]"
  private final val ERR_MISSING_PAREN: String =
    "missing closing )"
  private final val ERR_MISSING_REPEAT_ARGUMENT: String =
    "missing argument to repetition operator"
  private final val ERR_TRAILING_BACKSLASH: String =
    "trailing backslash at end of expression"

  // Hack to expose ArrayList.removeRange().
  private class Stack() extends ArrayList[Regexp]() {
    override def get(index: Int): Regexp = super.get(index)
    override def removeRange(fromIndex: Int, toIndex: Int): Unit =
      super.removeRange(fromIndex, toIndex)
  }

  // minFoldRune returns the minimum rune fold-equivalent to r.
  private def minFoldRune(_r: Int): Int = {
    var r: Int = _r
    if (r < Unicode.MIN_FOLD || r > Unicode.MAX_FOLD) {
      return r
    }
    var min: Int = r
    var r0: Int  = r
    r = Unicode.simpleFold(r)
    while (r != r0) {
      if (min > r) {
        min = r
      }
      r = Unicode.simpleFold(r)
    }
    return min
  }

  // leadingRegexp returns the leading regexp that re begins with.
  // The regexp refers to storage in re or its children.
  private def leadingRegexp(re: Regexp): Regexp = {
    if (re.op == ROP.EMPTY_MATCH) {
      return null
    }
    if (re.op == ROP.CONCAT && re.subs.length > 0) {
      val sub: Regexp = re.subs(0)
      if (sub.op == ROP.EMPTY_MATCH) {
        return null
      }
      return sub
    }
    return re
  }

  private def literalRegexp(s: String, flags: Int): Regexp = {
    val re: Regexp = new Regexp()
    re.op = ROP.LITERAL
    re.flags = flags
    re.runes = Utils.stringToRunes(s)
    re
  }

  // StringIterator: a stream of runes with an opaque cursor, permitting
  // rewinding.  The units of the cursor are not specified beyond the
  // fact that ASCII characters are single width.  (Cursor positions
  // could be UTF-8 byte indices, UTF-16 code indices or rune indices.)
  //
  // In particular, be careful with:
  // - skip(int): only use this to advance over ASCII characters
  //   since these always have a width of 1.
  // - skip(String): only use this to advance over strings which are
  //   known to be at the current position, e.g. due to prior call to
  //   lookingAt().
  // Only use pop() to advance over possibly non-ASCII runes.
  private class StringIterator(private val str: String) {
    private var _pos: Int = 0 // current position in UTF-16 string

    // Returns the cursor position.  Do not interpret the result!
    def pos(): Int = _pos

    // Resets the cursor position to a previous value returned by pos().
    def rewindTo(pos: Int): Unit = this._pos = pos

    // Returns true unless the stream is exhausted.
    def more(): Boolean = _pos < str.length()

    // Returns the rune at the cursor position.
    // Precondition: |more()|.
    def peek(): Int = str.codePointAt(_pos)

    // Advances the cursor by |n| positions, which must be ASCII runes.
    //
    // (In practise, this is only ever used to skip over regexp
    // metacharacters that are ASCII, so there is no numeric difference
    // between indices into  UTF-8 bytes, UTF-16 codes and runes.)
    def skip(n: Int): Unit = _pos += n

    // Advances the cursor by the number of cursor positions in |s|.
    def skipString(s: String): Unit = _pos += s.length()

    // Returns the rune at the cursor position, and advances the cursor
    // past it.  Precondition: |more()|.
    def pop(): Int = {
      val r: Int = str.codePointAt(_pos)
      _pos += Character.charCount(r)
      r
    }

    // Equivalent to both peek() == c but more efficient because we
    // don't support surrogates.  Precondition: |more()|.
    def lookingAt_c(c: Char): Boolean = str.charAt(_pos) == c

    // Equivalent to rest().startsWith(s).
    def lookingAt_s(s: String): Boolean = rest().startsWith(s)

    // Returns the rest of the pattern as a Java UTF-16 string.
    def rest(): String = str.substring(_pos)

    // Returns the substring from |beforePos| to the current position.
    // |beforePos| must have been previously returned by |pos()|.
    def from(beforePos: Int): String = str.substring(beforePos, _pos)

    override def toString(): String = rest()
  }

  /**
   * Parse regular expression pattern {@var pattern} with mode flags
   * {@var flags}.
   */
  def parse(pattern: String, flags: Int): Regexp =
    (new Parser(pattern, flags)).parseInternal()

  // parseRepeat parses {min} (max=min) or {min,} (max=-1) or {min,max}.
  // If |t| is not of that form, it returns -1.
  // If |t| has the right form but the values are negative or too big,
  // it returns -2.
  // On success, returns a nonnegative number encoding min/max in the
  // high/low signed halfwords of the result.  (Note: min >= 0 max may
  // be -1.)
  //
  // On success, advances |t| beyond the repeat otherwise |t.pos()| is
  // undefined.
  private def parseRepeat(t: StringIterator): Int = {
    val start: Int = t.pos()
    if (!t.more() || !t.lookingAt_c('{')) {
      return -1
    }
    t.skip(1) // '{'
    val min: Int = parseInt(t) // (can be -2)
    if (min == -1) {
      return -1
    }
    if (!t.more()) {
      return -1
    }
    var max: Int = 0
    if (!t.lookingAt_c(',')) {
      max = min
    } else {
      t.skip(1) // ','
      if (!t.more()) {
        return -1
      }
      if (t.lookingAt_c('}')) {
        max = -1
      } else if ({ max = parseInt(t); max } == -1) { // (can be -2)
        return -1
      }
    }
    if (!t.more() || !t.lookingAt_c('}')) {
      return -1
    }
    t.skip(1) // '}'
    if (min < 0 || min > 1000 ||
        max == -2 || max > 1000 || max >= 0 && min > max) {
      // Numbers were negative or too big, or max is present and min > max.
      throw new PatternSyntaxException(ERR_INVALID_REPEAT_SIZE, t.from(start))
    }
    return (min << 16) | (max & 0xffff) // success
  }

  // isValidCaptureName reports whether name
  // is a valid capture name: [A-Za-z0-9_]+.
  // PCRE limits names to 32 bytes.
  // Python rejects names starting with digits.
  // We don't enforce either of those.
  private def isValidCaptureName(name: String): Boolean = {
    if (name.isEmpty()) {
      return false
    }
    var i: Int = 0
    while (i < name.length()) {
      val c: Char = name.charAt(i)
      if (c != '_' && !Utils.isalnum(c)) {
        return false
      }
      i += 1
    }
    return true
  }

  // parseInt parses a nonnegative decimal integer.
  // -1 => bad format.  -2 => format ok, but integer overflow.
  private def parseInt(t: StringIterator): Int = {
    var start: Int = t.pos()
    var c: Int     = 0
    while (t.more() && { c = t.peek(); c } >= '0' && c <= '9') {
      t.skip(1) // digit
    }
    val n: String = t.from(start)
    if (n.isEmpty() ||
        n.length() > 1 && n.charAt(0) == '0') { // disallow leading zeros
      return -1 // bad format
    }
    if (n.length() > 8) {
      return -2 // overflow
    }
    return Integer.valueOf(n, 10) // can't fail
  }

  // can this be represented as a character class?
  // single-rune literal string, char class, ., and .|\n.
  private def isCharClass(re: Regexp): Boolean =
    (re.op == ROP.LITERAL && re.runes.length == 1 ||
      re.op == ROP.CHAR_CLASS ||
      re.op == ROP.ANY_CHAR_NOT_NL ||
      re.op == ROP.ANY_CHAR)

  // does re match r?
  private def matchRune(re: Regexp, r: Int): Boolean = {
    re.op match {
      case ROP.LITERAL =>
        re.runes.length == 1 && re.runes(0) == r
      case ROP.CHAR_CLASS =>
        var i: Int = 0
        while (i < re.runes.length) {
          if (re.runes(i) <= r && r <= re.runes(i + 1)) {
            return true
          }
          i += 2
        }
        false
      case ROP.ANY_CHAR_NOT_NL =>
        r != '\n'
      case ROP.ANY_CHAR =>
        true
      case _ =>
        false
    }
  }

  // mergeCharClass makes dst = dst|src.
  // The caller must ensure that dst.Op >= src.Op,
  // to reduce the amount of copying.
  private def mergeCharClass(dst: Regexp, src: Regexp): Unit = {
    dst.op match {
      case ROP.ANY_CHAR =>
        // src doesn't add anything.
        ()
      case ROP.ANY_CHAR_NOT_NL =>
        // src might add \n
        if (matchRune(src, '\n')) {
          dst.op = ROP.ANY_CHAR
        }
      case ROP.CHAR_CLASS =>
        // src is simpler, so either literal or char class
        if (src.op == ROP.LITERAL) {
          dst.runes = new CharClass(dst.runes)
            .appendLiteral(src.runes(0), src.flags)
            .toArray()
        } else {
          dst.runes = new CharClass(dst.runes).appendClass(src.runes).toArray()
        }
      case ROP.LITERAL =>
        // both literal
        if (src.runes(0) == dst.runes(0) && src.flags == dst.flags) {
          ()
        } else {
          dst.op = ROP.CHAR_CLASS
          dst.runes = new CharClass(Utils.EMPTY_INTS)
            .appendLiteral(dst.runes(0), dst.flags)
            .appendLiteral(src.runes(0), src.flags)
            .toArray()
        }
    }
  }

  // parseEscape parses an escape sequence at the beginning of s
  // and returns the rune.
  // Pre: t at '\\'.  Post: after escape.
  private def parseEscape(t: StringIterator): Int = {
    val startPos: Int = t.pos()
    t.skip(1) // '\\'
    if (!t.more()) {
      throw new PatternSyntaxException(ERR_TRAILING_BACKSLASH, "")
    }
    var c: Int = t.pop()

    c match {
      // Octal escapes.
      case '1' | '2' | '3' | '4' | '5' | '6' | '7'
          // Single non-zero digit is a backreference not supported
          if !t.more() || t.peek() < '0' || t.peek() > '7' =>
        throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))

      // Consume up to three octal digits already have one.
      case '0' =>
        var r: Int          = c - '0'
        var i: Int          = 1
        var breakInner: Boolean = false
        while (i < 3 && !breakInner) {
          if (!t.more() || t.peek() < '0' || t.peek() > '7') {
            breakInner = true
          } else {
            r = r * 8 + t.peek() - '0'
            t.skip(1) // digit
            i += 1
          }
        }
        r

      // Hexadecimal escapes.
      case 'x' =>
        if (!t.more()) {
          throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
        }
        c = t.pop()
        if (c == '{') {
          // Any number of digits in braces.
          // Perl accepts any text at all it ignores all text
          // after the first non-hex digit.  We require only hex digits,
          // and at least one.
          var nhex: Int       = 0
          var r: Int          = 0
          var breakInner: Boolean = false
          while (!breakInner) {
            if (!t.more()) {
              throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
            }
            c = t.pop()
            if (c == '}') {
              breakInner = true
            }
            if (!breakInner) {
              val v: Int = Utils.unhex(c)
              if (v < 0) {
                throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
              }
              r = r * 16 + v
              if (r > Unicode.MAX_RUNE) {
                throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
              }
              nhex += 1
            }
          }
          if (nhex == 0) {
            throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
          }
          r
        } else {
          // Easy case: two hex digits.
          val x: Int = Utils.unhex(c)
          c = t.pop()
          val y: Int = Utils.unhex(c)
          if (x < 0 || y < 0) {
            throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
          }
          x * 16 + y
        }

      // C escapes.  There is no case 'b', to avoid misparsing
      // the Perl word-boundary \b as the C backspace \b
      // when in POSIX mode.  In Perl, /\b/ means word-boundary
      // but /[\b]/ means backspace.  We don't support that.
      // If you want a backspace, embed a literal backspace
      // character or use \x08.
      case 'a' =>
        7 // No \a in Java
      case 'f' =>
        '\f'
      case 'n' =>
        '\n'
      case 'r' =>
        '\r'
      case 't' =>
        '\t'
      case 'v' =>
        11 // No \v in Java

      // Default case.
      case _ =>
        if (!Utils.isalnum(c)) {
          // Escaped non-word characters are always themselves.
          // PCRE is not quite so rigorous: it accepts things like
          // \q, but we don't.  We once rejected \_, but too many
          // programs and people insist on using it, so allow \_.
          c
        } else {
          throw new PatternSyntaxException(ERR_INVALID_ESCAPE, t.from(startPos))
        }
    }
  }

  // parseClassChar parses a character class character and returns it.
  // wholeClassPos is the position of the start of the entire class "[...".
  // Pre: t at class char Post: t after it.
  private def parseClassChar(t: StringIterator, wholeClassPos: Int): Int = {
    if (!t.more()) {
      throw new PatternSyntaxException(ERR_MISSING_BRACKET,
                                       t.from(wholeClassPos))
    }

    // Allow regular escape sequences even though
    // many need not be escaped in this context.
    if (t.lookingAt_c('\\')) {
      return parseEscape(t)
    }

    return t.pop()
  }

  // RangeTables are represented as int[][], a list of triples (start, end,
  // stride).
  private val ANY_TABLE: Array[Array[Int]] = Array[Array[Int]](
    Array[Int](0, Unicode.MAX_RUNE, 1)
  )

  // unicodeTable() returns the Unicode RangeTable identified by name
  // and the table of additional fold-equivalent code points.
  // Returns null if |name| does not identify a Unicode character range.
  private def unicodeTable(
      name: String): Pair[Array[Array[Int]], Array[Array[Int]]] = {
    // Special case: "Any" means any.
    if (name.equals("Any")) {
      return Pair.of[Array[Array[Int]], Array[Array[Int]]](ANY_TABLE, ANY_TABLE)
    }
    var table: Array[Array[Int]] = UnicodeTables.CATEGORIES.get(name)
    if (table != null) {
      return Pair.of[Array[Array[Int]], Array[Array[Int]]](table, UnicodeTables.FOLD_CATEGORIES.get(name))
    }
    table = UnicodeTables.SCRIPTS.get(name)
    if (table != null) {
      return Pair.of[Array[Array[Int]], Array[Array[Int]]](table, UnicodeTables.FOLD_SCRIPT.get(name))
    }
    return null
  }

  //// Utilities

  // Returns a new copy of the specified subarray.
  def subarray(array: Array[Regexp], start: Int, end: Int): Array[Regexp] = {
    val r: Array[Regexp] = new Array[Regexp](end - start)
    var i: Int = start
    while (i < end) {
      r(i - start) = array(i)
      i += 1
    }
    r
  }

  private class Pair[F, S](val first: F, val second: S)

  private object Pair {
    def of[F, S](f: F, s: S): Pair[F, S] = new Pair(f, s)
  }

  private def concatRunes(x: Array[Int], y: Array[Int]): Array[Int] = {
    val z: Array[Int] = new Array[Int](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }
}
