package dotty.tools.languageserver.util.embedded

import dotty.tools.languageserver.util.CodeRange

/**
 * Wrapper for some text that appears within a range.
 *
 * @param text  The text that is comprised inside `range`.
 * @param range The range of positions that encloses the `text`.
 */
case class CodeInRange(text: String, range: CodeRange) extends Embedded
