package dotty.tools.scaladoc.tasty.comments

import java.util.regex.Matcher
import Regexes._

object Cleaner {

  /** Prepares the comment for pre-parsing: removes documentation markers and
    * extra whitespace, removes dangerous HTML and Javadoc tags, and splits it
    * into lines.
    */
  def clean(comment: String): List[String] = {
    def cleanLine(line: String): String = {
      line match {
        case CleanCommentLine(ctl: String) => ctl
        // This match can actually never happen, because CleanCommentLine always matches
        case tl => tl
      }
    }
    val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
    val safeComment = DangerousTags.replaceAllIn(strippedComment, htmlReplacement)
    val javadoclessComment = JavadocTags.replaceAllIn(safeComment, javadocReplacement)
    val markedTagComment =
      SafeTags.replaceAllIn(javadoclessComment, { mtch =>
        Matcher.quoteReplacement(s"$safeTagMarker${mtch.matched}$safeTagMarker")
      })
    markedTagComment.linesIterator.toList.map(cleanLine)
  }
}
