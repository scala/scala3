package dotty.tools.scaladoc
package tasty.comments

object Cleaner {
  import Regexes._
  import java.util.regex.Matcher

  /** Prepares the comment for pre-parsing: removes documentation markers and
    * extra whitespace, removes dangerous HTML and Javadoc tags, and splits it
    * into lines.
    */
  def clean(comment: String): List[String] = {
    def cleanLine(line: String): String = {
      // Remove trailing whitespaces
      TrailingWhitespace.replaceAllIn(line, "") match {
        case CleanCommentLine(ctl: String) => ctl
        case tl => tl
      }
    }
    val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
    val safeComment = DangerousTags.replaceAllIn(strippedComment, { htmlReplacement(_) })
    val javadoclessComment = JavadocTags.replaceAllIn(safeComment, { javadocReplacement(_) })
    val markedTagComment =
      SafeTags.replaceAllIn(javadoclessComment, { mtch =>
        Matcher.quoteReplacement(s"$safeTagMarker${mtch.matched}$safeTagMarker")
      })
    markedTagComment.linesIterator.toList map (cleanLine)
  }
}
