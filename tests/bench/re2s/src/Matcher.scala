// Copyright 2010 Google Inc. All Rights Reserved.

package java.util.regex

/**
 * A stateful iterator that interprets a regex {@code Pattern} on a
 * specific input.  Its interface mimics the JDK 1.4.2
 * {@code java.util.regex.Matcher}.
 *
 * <p>Conceptually, a Matcher consists of four parts:
 * <ol>
 *   <li>A compiled regular expression {@code Pattern}, set at
 *   construction and fixed for the lifetime of the matcher.</li>
 *
 *   <li>The remainder of the input string, set at construction or
 *   {@link #reset_0()} and advanced by each match operation such as
 *   {@link #find}, {@link #matches} or {@link #lookingAt}.</li>
 *
 *   <li>The current match information, accessible via {@link #start},
 *   {@link #end}, and {@link #group}, and updated by each match
 *   operation.</li>
 *
 *   <li>The append position, used and advanced by
 *   {@link #appendReplacement} and {@link #appendTail} if performing a
 *   search and replace from the input to an external {@code StringBuffer}.
 *
 * </ol>
 *
 * <p>See the <a href="package.html">package-level
 * documentation</a> for an overview of how to use this API.</p>
 *
 * @author rsc@google.com (Russ Cox)
 */
final class Matcher(private val _pattern: Pattern, private var _inputSequence: CharSequence) {
  if (_pattern == null) {
    throw new NullPointerException("pattern is null")
  }

  // The number of submatches (groups) in the pattern.
  private val _groupCount: Int = _pattern.re2.numberOfCapturingGroups()

  // The group indexes, in [start, end) pairs.  Zeroth pair is overall match.
  private val _groups: Array[Int] = new Array[Int](2 + 2 * _groupCount)

  // The input length in UTF16 codes.
  private var _inputLength: Int = _inputSequence.length()

  // The append position: where the next append should start.
  private var _appendPos: Int = 0

  // Is there a current match?
  private var _hasMatch: Boolean = false

  // Have we found the submatches (groups) of the current match?
  // group[0], group[1] are set regardless.
  private var _hasGroups: Boolean = false

  // The anchor flag to use when repeating the match to find subgroups.
  private var _anchorFlag: Int = 0

  /** Returns the {@code Pattern} associated with this {@code Matcher}. */
  def pattern(): Pattern = _pattern

  /**
   * Resets the {@code Matcher}, rewinding input and
   * discarding any match information.
   *
   * @return the {@code Matcher} itself, for chained method calls
   */
  def reset_0(): Matcher = {
    _appendPos = 0
    _hasMatch = false
    _hasGroups = false
    this
  }

  /**
   * Resets the {@code Matcher} and changes the input.
   *
   * @param input the new input string
   * @return the {@code Matcher} itself, for chained method calls
   */
  def reset_1(input: CharSequence): Matcher = {
    if (input == null) {
      throw new NullPointerException("input is null")
    }
    reset_0()
    _inputSequence = input
    _inputLength = input.length()
    this
  }

  /**
   * Returns the start position of the most recent match.
   *
   * @throws IllegalStateException if there is no match
   */
  def start_0(): Int = start_1(0)

  /**
   * Returns the end position of the most recent match.
   *
   * @throws IllegalStateException if there is no match
   */
  def end_0(): Int = end_1(0)

  /**
   * Returns the start position of a subgroup of the most recent match.
   *
   * @param group the group index 0 is the overall match
   * @throws IllegalStateException if there is no match
   * @throws IndexOutOfBoundsException
   *   if {@code group < 0} or {@code group > groupCount()}
   */
  def start_1(group: Int): Int = {
    loadGroup(group)
    _groups(2 * group)
  }

  /**
   * Returns the end position of a subgroup of the most recent match.
   *
   * @param group the group index 0 is the overall match
   * @throws IllegalStateException if there is no match
   * @throws IndexOutOfBoundsException
   *   if {@code group < 0} or {@code group > groupCount()}
   */
  def end_1(group: Int): Int = {
    loadGroup(group)
    _groups(2 * group + 1)
  }

  /**
   * Returns the most recent match.
   *
   * @throws IllegalStateException if there is no match
   */
  def group_0(): String = group_1(0)

  /**
   * Returns the subgroup of the most recent match.
   *
   * @throws IllegalStateException if there is no match
   * @throws IndexOutOfBoundsException if {@code group < 0}
   *   or {@code group > groupCount()}
   */
  def group_1(group: Int): String = {
    val start: Int = this.start_1(group)
    val end: Int   = this.end_1(group)
    if (start < 0 && end < 0) {
      // Means the subpattern didn't get matched at all.
      return null
    }
    substring(start, end)
  }

  /**
   * Returns the number of subgroups in this pattern.
   *
   * @return the number of subgroups the overall match (group 0) does not count
   */
  def groupCount(): Int = _groupCount

  /** Helper: finds subgroup information if needed for group. */
  private def loadGroup(group: Int): Unit = {
    if (group < 0 || group > _groupCount) {
      throw new IndexOutOfBoundsException(
        "Group index out of bounds: " + group)
    }
    if (!_hasMatch) {
      throw new IllegalStateException("perhaps no match attempted")
    }
    if (group == 0 || _hasGroups) {
      return
    }

    // Include the character after the matched text (if there is one).
    // This is necessary in the case of inputSequence abc and pattern
    // (a)(b$)?(b)? . If we do pass in the trailing c,
    // the groups evaluate to new String[] {"ab", "a", null, "b" }
    // If we don't, they evaluate to new String[] {"ab", "a", "b", null}
    // We know it won't affect the total matched because the previous call
    // to match included the extra character, and it was not matched then.
    var end: Int = _groups(1) + 1
    if (end > _inputLength) {
      end = _inputLength
    }

    val ok: Boolean = _pattern.re2.match_5(_inputSequence,
                                 _groups(0),
                                 end,
                                 _anchorFlag,
                                 _groups,
                                 1 + _groupCount)
    // Must match - hasMatch says that the last call with these
    // parameters worked just fine.
    if (!ok) {
      throw new IllegalStateException("inconsistency in matching group data")
    } else {
      _hasGroups = true
    }
  }

  /**
   * Matches the entire input against the pattern (anchored start and end).
   * If there is a match, {@code matches} sets the match state to describe it.
   *
   * @return true if the entire input matches the pattern
   */
  def matches(): Boolean = genMatch(0, RE2.ANCHOR_BOTH)

  /**
   * Matches the beginning of input against the pattern (anchored start).
   * If there is a match, {@code lookingAt} sets the match state to describe it.
   *
   * @return true if the beginning of the input matches the pattern
   */
  def lookingAt(): Boolean = genMatch(0, RE2.ANCHOR_START)

  /**
   * Matches the input against the pattern (unanchored).
   * The search begins at the end of the last match, or else the beginning
   * of the input.
   * If there is a match, {@code find} sets the match state to describe it.
   *
   * @return true if it finds a match
   */
  def find_0(): Boolean = {
    var start: Int = 0
    if (_hasMatch) {
      start = _groups(1)
      if (_groups(0) == _groups(1)) { // empty match - nudge forward
        start += 1
      }
    }
    genMatch(start, RE2.UNANCHORED)
  }

  /**
   * Matches the input against the pattern (unanchored),
   * starting at a specified position.
   * If there is a match, {@code find} sets the match state to describe it.
   *
   * @param start the input position where the search begins
   * @return true if it finds a match
   * @throws IndexOutOfBoundsException if start is not a valid input position
   */
  def find_1(start: Int): Boolean = {
    if (start < 0 || start > _inputLength) {
      throw new IndexOutOfBoundsException(
        "start index out of bounds: " + start)
    }
    reset_0()
    genMatch(start, 0)
  }

  /** Helper: does match starting at start, with RE2 anchor flag. */
  private def genMatch(startByte: Int, anchor: Int): Boolean = {
    val ok: Boolean = _pattern.re2.match_5(_inputSequence,
                                 startByte,
                                 _inputLength,
                                 anchor,
                                 _groups,
                                 1)
    if (!ok) {
      false
    } else {
      _hasMatch = true
      _hasGroups = false
      _anchorFlag = anchor
      true
    }
  }

  /** Helper: return substring for [start, end). */
  def substring(start: Int, end: Int): String =
    // This is fast for both StringBuilder and String.
    _inputSequence.subSequence(start, end).toString()

  /** Helper for Pattern: return input length. */
  def inputLength(): Int =
    _inputLength

  /**
   * Appends to {@code sb} two strings: the text from the append position up
   * to the beginning of the most recent match, and then the replacement with
   * submatch groups substituted for references of the form {@code $n}, where
   * {@code n} is the group number in decimal.  It advances the append position
   * to the position where the most recent match ended.
   *
   * <p>To embed a literal {@code $}, use \$ (actually {@code "\\$"} with string
   * escapes).  The escape is only necessary when {@code $} is followed by a
   * digit, but it is always allowed.  Only {@code $} and {@code \} need
   * escaping, but any character can be escaped.
   *
   * <p>The group number {@code n} in {@code $n} is always at least one digit
   * and expands to use more digits as long as the resulting number is a
   * valid group number for this pattern.  To cut it off earlier, escape the
   * first digit that should not be used.
   *
   * @param sb the {@link StringBuffer} to append to
   * @param replacement the replacement string
   * @return the {@code Matcher} itself, for chained method calls
   * @throws IllegalStateException if there was no most recent match
   * @throws IndexOutOfBoundsException if replacement refers to an invalid group
   */
  def appendReplacement(sb: StringBuffer, replacement: String): Matcher = {
    val s: Int = start_0()
    val e: Int = end_0()
    if (_appendPos < s) {
      sb.append(substring(_appendPos, s))
    }
    _appendPos = e
    var last: Int = 0
    var i: Int    = 0
    val m: Int    = replacement.length()
    while (i < m - 1) {
      if (replacement.charAt(i) == '\\') {
        if (last < i) {
          sb.append(replacement.substring(last, i))
        }
        i += 1
        last = i
      } else if (replacement.charAt(i) == '$') {
        var c: Char = replacement.charAt(i + 1)
        if ('0' <= c && c <= '9') {
          var n: Int = c - '0'
          if (last < i) {
            sb.append(replacement.substring(last, i))
          }
          i += 2
          var break: Boolean = false
          while (!break && i < m) {
            c = replacement.charAt(i)
            if (c < '0' || c > '9' || n * 10 + c - '0' > _groupCount) {
              break = true
            } else {
              n = n * 10 + c - '0'
              i += 1
            }
          }
          if (n > _groupCount) {
            throw new IndexOutOfBoundsException("n > number of groups: " + n)
          }
          val group: String = this.group_1(n)
          if (group != null) {
            sb.append(group)
          }
          last = i
          i -= 1
        }
      }
      i += 1
    }
    if (last < m) {
      sb.append(replacement.substring(last, m))
    }
    this
  }

  /**
   * Appends to {@code sb} the substring of the input from the
   * append position to the end of the input.
   *
   * @param sb the {@link StringBuffer} to append to
   * @return the argument {@code sb}, for method chaining
   */
  def appendTail(sb: StringBuffer): StringBuffer = {
    sb.append(substring(_appendPos, _inputLength))
    sb
  }

  /**
   * Returns the input with all matches replaced by {@code replacement},
   * interpreted as for {@code appendReplacement}.
   *
   * @param replacement the replacement string
   * @return the input string with the matches replaced
   * @throws IndexOutOfBoundsException if replacement refers to an invalid group
   */
  def replaceAll(replacement: String): String =
    replace(replacement, true)

  /**
   * Returns the input with the first match replaced by {@code replacement},
   * interpreted as for {@code appendReplacement}.
   *
   * @param replacement the replacement string
   * @return the input string with the first match replaced
   * @throws IndexOutOfBoundsException if replacement refers to an invalid group
   */
  def replaceFirst(replacement: String): String =
    replace(replacement, false)

  /** Helper: replaceAll/replaceFirst hybrid. */
  private def replace(replacement: String, all: Boolean): String = {
    reset_0()
    val sb: StringBuffer    = new StringBuffer()
    var break: Boolean = false
    while (!break && find_0()) {
      appendReplacement(sb, replacement)
      if (!all) {
        break = true
      }
    }
    appendTail(sb)
    sb.toString()
  }
}

object Matcher {

  /**
   * Quotes '\' and '$' in {@code s}, so that the returned string could be
   * used in {@link #appendReplacement} as a literal replacement of {@code s}.
   *
   * @param s the string to be quoted
   * @return the quoted string
   */
  def quoteReplacement(s: String): String = {
    if (s.indexOf('\\') < 0 && s.indexOf('$') < 0) {
      return s
    }
    val sb: java.lang.StringBuilder = new java.lang.StringBuilder()
    var i: Int  = 0
    while (i < s.length()) {
      val c: Char = s.charAt(i)
      if (c == '\\' || c == '$') {
        sb.append('\\')
      }
      sb.append(c)
      i += 1
    }
    sb.toString
  }
}
