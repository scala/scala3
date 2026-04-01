package dotty.tools.scaladoc
package tasty.comments

object Cleaner {
  import Regexes._
  import java.util.regex.Matcher

  // Tags that are considered safe enough and do not need escaping
  private val SafeTags = Set(
    "a", "abbr", "address", "area", "blockquote", "br", "b", "caption", "cite", "code", "col", "colgroup",
    "dd", "del", "dfn", "em", "hr", "img", "ins", "i", "kbd", "label", "legend", "pre", "q", "samp",
    "small", "span", "strong", "sub", "sup", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "var"
  )

  /** Removes all tags except simple ones that can be translated or that are definitely harmless.
   *  Not the fastest code in the world, but should work just fine for our purposes. */
  private def cleanHtml(text: String): String = {
    val result = StringBuilder()
    var index = 0
    while (index < text.length) {
      if (text(index) == safeTagMarker) {
        // ignore it, it's a character that should never appear in everyday text anyway
        index += 1
      } else if (text(index) == '<') {
        val endOfTagIndex = text.indexOf('>', index)
        if (endOfTagIndex == -1) {
          return result.toString
        }
        val endOfNameIndex = text.indexOf(' ', index)
        val subStringEndIndex = if (endOfNameIndex == -1) endOfTagIndex else Math.min(endOfTagIndex, endOfNameIndex)
        result.append(text.substring(index + 1, subStringEndIndex) match {
          case "p" | "div" => "\n\n"
          case "h1"  => "\n= "
          case "/h1" => " =\n"
          case "h2"  => "\n== "
          case "/h2" => " ==\n"
          case "h3"  => "\n=== "
          case "/h3" => " ===\n"
          case "h4" | "h5" | "h6" => "\n==== "
          case "/h4" | "/h5" | "/h6" => " ====\n"
          case "li" => "\n *  - "
          case "/li" => ""
          case "" => ""
          case other =>
            val simple = if (other(0) == '/') other.substring(1) else other
            if (SafeTags(simple)) {
              s"$safeTagMarker${text.substring(index, endOfTagIndex + 1)}$safeTagMarker"
            } else {
              ""
            }
        })
        index = endOfTagIndex + 1
      } else {
        result.append(text(index))
        index += 1
      }
    }
    result.toString
  }

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
    val safeComment = cleanHtml(strippedComment)
    val javadoclessComment = JavadocTags.replaceAllIn(safeComment, javadocReplacement)
    javadoclessComment.linesIterator.toList.map(cleanLine)
  }
}
