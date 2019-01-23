/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dottydoc
package model
package comment

trait CommentCleaner {
  import Regexes._
  import java.util.regex.Matcher

  def clean(comment: String): List[String] = {
    def cleanLine(line: String): String = {
      // Remove trailing whitespaces
      TrailingWhitespace.replaceAllIn(line, "") match {
        case CleanCommentLine(ctl) => ctl
        case tl => tl
      }
    }
    val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
    val safeComment = DangerousTags.replaceAllIn(strippedComment, { htmlReplacement(_) })
    val javadoclessComment = JavadocTags.replaceAllIn(safeComment, { javadocReplacement(_) })
    val markedTagComment =
      SafeTags.replaceAllIn(javadoclessComment, { mtch =>
        Matcher.quoteReplacement(safeTagMarker + mtch.matched + safeTagMarker)
      })
    markedTagComment.linesIterator.toList map (cleanLine)
  }
}
