// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package java.util.regex

/**
 * An exception thrown by the parser if the pattern was invalid.
 *
 * Following {@code java.util.regex.PatternSyntaxException}, this is an
 * unchecked exception.
 */
class PatternSyntaxException(error: String, input: String)
    extends RuntimeException(
      "error parsing regexp: " + error + ": `" + input + "`") {
  /**
   * Retrieves the error index.
   *
   * @return  The approximate index in the pattern of the error,
   *         or <tt>-1</tt> if the index is not known
   */
  def getIndex(): Int = -1

  /**
   * Retrieves the description of the error.
   *
   * @return  The description of the error
   */
  def getDescription(): String = error

  /**
   * Retrieves the erroneous regular-expression pattern.
   *
   * @return  The erroneous pattern
   */
  def getPattern(): String = input
}
