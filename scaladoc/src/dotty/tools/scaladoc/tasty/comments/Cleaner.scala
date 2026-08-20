package dotty.tools.scaladoc
package tasty.comments

/**
 * Removes HTML tags except simple ones that can be translated or that are definitely harmless,
 * translates Scala/Javadoc tags, and generally cleans the input.
 * Not the fastest code in the world, and should eventually be replaced by a real parser,
 * but works fine for now. */
object Cleaner {
  import Regexes._
  import java.util.regex.Matcher

  // Tags that are considered safe enough and do not need escaping
  private val SafeTags = Set(
    "a", "abbr", "address", "area", "blockquote", "br", "b", "caption", "cite", "code", "col", "colgroup",
    "dd", "del", "dfn", "em", "hr", "img", "ins", "i", "kbd", "label", "legend", "pre", "q", "samp",
    "small", "span", "strong", "sub", "sup", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "var"
  )

  private def cleanHtml(text: String): String = {
    val result = StringBuilder()
    var index = 0
    var insideCode = false
    var insideLink = false
    while (index < text.length) {
      if (insideCode) {
        result.append(text(index))
        if (index >= 2 && text(index) == '`' && text(index - 1) == '`' && text(index - 2) == '`') {
          insideCode = false
        }
        index += 1
      } else if (insideLink) {
        result.append(text(index))
        if (index >= 1 && text(index) == ']' && text(index - 1) == ']') {
          insideLink = false
        }
        index += 1
      } else if (index <= text.length - 3 && text(index) == '`' && text(index + 1) == '`' && text(index + 2) == '`') {
        result.append("```")
        insideCode = true
        index += 3
      } else if (index <= text.length - 2 && text(index) == '[' && text(index + 1) == '[') {
        result.append("[[")
        insideLink = true
        index += 2
      } else if (text(index) == safeTagMarker) {
        // ignore it, it's a character that should never appear in everyday text anyway
        index += 1
      } else if (text(index) == '<') {
        val endOfNameIndex = text.indexOf(' ', index)
        val endOfTagIndex = text.indexOf('>', index)
        if (endOfNameIndex == -1 || endOfNameIndex == index + 1 || endOfTagIndex == -1) {
          // not actually a tag, e.g., "< hello >", "a<b"
          result.append("&lt;")
          index += 1
        } else {
          val subStringEndIndex = Math.min(endOfTagIndex, endOfNameIndex)
          result.append(text.substring(index + 1, subStringEndIndex) match {
            case "" => "&lt;" // not actually a tag
            case "p" | "div" => "\n\n"
            case "h1" => "\n= "
            case "/h1" => " =\n"
            case "h2" => "\n== "
            case "/h2" => " ==\n"
            case "h3" => "\n=== "
            case "/h3" => " ===\n"
            case "h4" | "h5" | "h6" => "\n==== "
            case "/h4" | "/h5" | "/h6" => " ====\n"
            case "li" => "\n *  - "
            case "/li" => ""
            case other =>
              val simple = if (other(0) == '/') other.substring(1) else other
              if (SafeTags(simple)) {
                s"$safeTagMarker${text.substring(index, endOfTagIndex + 1)}$safeTagMarker"
              } else {
                ""
              }
          })
          index = endOfTagIndex + 1
        }
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
