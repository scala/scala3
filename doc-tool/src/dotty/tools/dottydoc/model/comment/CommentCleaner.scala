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
        Matcher.quoteReplacement(s"${safeTagMarker}${mtch.matched}${safeTagMarker}")
      })
    markedTagComment.linesIterator.toList map (cleanLine)
  }
}
