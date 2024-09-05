// Copyright 2010 Google Inc. All Rights Reserved.

package java.util.regex

import java.io.Serializable

/**
 * A compiled representation of an RE2 regular expression, mimicking the
 * {@code java.util.regex.Pattern} API.
 *
 * <p>The matching functions take {@code String} arguments instead of
 * the more general Java {@code CharSequence} since the latter doesn't
 * provide UTF-16 decoding.
 *
 * <p>See the <a href='package.html'>package-level
 * documentation</a> for an overview of how to use this API.</p>
 *
 * @author rsc@google.com (Russ Cox)
 */
final class Pattern(val pattern: String, val flags: Int, val re2: RE2)
    extends Serializable() {
  if (pattern == null) {
    throw new NullPointerException("pattern is null")
  }
  if (re2 == null) {
    throw new NullPointerException("re2 is null")
  }

  /**
   * Releases memory used by internal caches associated with this pattern. Does
   * not change the observable behaviour. Useful for tests that detect memory
   * leaks via allocation tracking.
   */
  def reset_0(): Unit = re2.reset_0()

  def matches(input: String): Boolean =
    matcher(input).matches()

  /**
   * Creates a new {@code Matcher} matching the pattern against the input.
   *
   * @param input the input string
   */
  def matcher(input: CharSequence): Matcher =
    new Matcher(this, input)

  /**
   * Splits input around instances of the regular expression.
   * It returns an array giving the strings that occur before, between, and after instances
   * of the regular expression.  Empty strings that would occur at the end
   * of the array are omitted.
   *
   * @param input the input string to be split
   * @return the split strings
   */
  def split_1(input: String): Array[String] =
    split_2(input, 0)

  /**
   * Splits input around instances of the regular expression.
   * It returns an array giving the strings that occur before, between, and after instances
   * of the regular expression.
   *
   * <p>If {@code limit <= 0}, there is no limit on the size of the returned array.
   * If {@code limit == 0}, empty strings that would occur at the end of the array are omitted.
   * If {@code limit > 0}, at most limit strings are returned.  The final string contains
   * the remainder of the input, possibly including additional matches of the pattern.
   *
   * @param input the input string to be split
   * @param limit the limit
   * @return the split strings
   */
  def split_2(input: String, limit: Int): Array[String] =
    split(matcher(input), limit)

  /** Helper: run split on m's input. */
  private def split(m: Matcher, limit: Int): Array[String] = {
    var matchCount: Int = 0
    var arraySize: Int  = 0
    var last: Int       = 0
    while (m.find_0()) {
      matchCount += 1
      if (limit != 0 || last < m.start_0()) {
        arraySize = matchCount
      }
      last = m.end_0()
    }
    if (last < m.inputLength() || limit != 0) {
      matchCount += 1
      arraySize = matchCount
    }

    var trunc: Int = 0
    if (limit > 0 && arraySize > limit) {
      arraySize = limit
      trunc = 1
    }
    val array: Array[String] = new Array[String](arraySize)
    var i: Int     = 0
    last = 0
    m.reset_0()
    while (m.find_0() && i < arraySize - trunc) {
      array(i) = m.substring(last, m.start_0())
      i += 1
      last = m.end_0()
    }
    if (i < arraySize) {
      array(i) = m.substring(last, m.inputLength())
    }
    array
  }

  override def toString(): String = pattern

  /**
   * Returns the number of capturing groups in this matcher's pattern.
   * Group zero denotes the entire pattern and is excluded from this count.
   *
   * @return the number of capturing groups in this pattern
   */
  def groupCount(): Int =
    re2.numberOfCapturingGroups()
}

object Pattern {

  /** Flag: case insensitive matching. */
  final val CASE_INSENSITIVE: Int = 1

  /** Flag: dot ({@code .}) matches all characters, including newline. */
  final val DOTALL: Int = 2

  /**
   * Flag: multiline matching: {@code ^} and {@code $} match at
   * beginning and end of line, not just beginning and end of input.
   */
  final val MULTILINE: Int = 4

  /**
   * Flag: Unicode groups (e.g. {@code \p\{Greek\}}) will be syntax errors.
   */
  final val DISABLE_UNICODE_GROUPS: Int = 8

  /**
   * Creates and returns a new {@code Pattern} corresponding to
   * compiling {@code regex} with the default flags (0).
   *
   * @param regex the regular expression
   * @throws PatternSyntaxException if the pattern is malformed
   */
  def compile_1(regex: String): Pattern =
    compile_3(regex, regex, 0)

  /**
   * Creates and returns a new {@code Pattern} corresponding to
   * compiling {@code regex} with the default flags (0).
   *
   * @param regex the regular expression
   * @param flags bitwise OR of the flag constants {@code CASE_INSENSITIVE},
   *    {@code DOTALL}, and {@code MULTILINE}
   * @throws PatternSyntaxException if the regular expression is malformed
   * @throws IllegalArgumentException if an unknown flag is given
   */
  def compile_2(regex: String, flags: Int): Pattern = {
    var flregex: String = regex
    if ((flags & CASE_INSENSITIVE) != 0) {
      flregex = "(?i)" + flregex
    }
    if ((flags & DOTALL) != 0) {
      flregex = "(?s)" + flregex
    }
    if ((flags & MULTILINE) != 0) {
      flregex = "(?m)" + flregex
    }
    if ((flags & ~(MULTILINE | DOTALL | CASE_INSENSITIVE | DISABLE_UNICODE_GROUPS)) != 0) {
      throw new IllegalArgumentException(
        "Flags should only be a combination " +
          "of MULTILINE, DOTALL, CASE_INSENSITIVE, DISABLE_UNICODE_GROUPS")
    }
    compile_3(flregex, regex, flags)
  }

  /**
   * Helper: create new Pattern with given regex and flags.
   * Flregex is the regex with flags applied.
   */
  private def compile_3(flregex: String, regex: String, flags: Int): Pattern = {
    var re2Flags: Int = RE2.PERL
    if ((flags & DISABLE_UNICODE_GROUPS) != 0) {
      re2Flags &= ~RE2.UNICODE_GROUPS
    }
    new Pattern(regex,
                flags,
                RE2.compileImpl(flregex, re2Flags, /*longest=*/ false))
  }

  /**
   * Matches a string against a regular expression.
   *
   * @param regex the regular expression
   * @param input the input
   * @return true if the regular expression matches the entire input
   * @throws PatternSyntaxException if the regular expression is malformed
   */
  def matches(regex: String, input: CharSequence): Boolean =
    compile_1(regex).matcher(input).matches()

  /**
   * Returns a literal pattern string for the specified
   * string.
   *
   * <p>This method produces a string that can be used to
   * create a <code>Pattern</code> that would match the string
   * <code>s</code> as if it were a literal pattern.</p> Metacharacters
   * or escape sequences in the input sequence will be given no special
   * meaning.
   *
   * @param s The string to be literalized
   * @return A literal string replacement
   */
  def quote(s: String): String =
    RE2.quoteMeta(s)
}
