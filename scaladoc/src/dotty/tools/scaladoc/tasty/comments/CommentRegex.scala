package dotty.tools.scaladoc
package tasty.comments

import scala.util.matching.Regex

object Regexes {
  val TrailingWhitespace = """\s+$""".r

  /** The body of a line, dropping the (optional) start star-marker,
    * one leading whitespace and all trailing whitespace
    */
  val CleanCommentLine =
    new Regex("""(?:\s*\*\s?\s?)?(.*)""")

  /** Javadoc tags that should be replaced by something useful, such as wiki
    * syntax, or that should be dropped. */
  val JavadocTags =
    new Regex("""\{\@(code|docRoot|linkplain|link|literal|value)\p{Zs}*([^}]*)\}""")

  /** Maps a javadoc tag to a useful wiki replacement, or an empty string if it cannot be salvaged. */
  def javadocReplacement(mtch: Regex.Match): String = {
    mtch.group(1) match {
      case "code" => "<code>" + mtch.group(2) + "</code>"
      case "docRoot"  => ""
      case "link"  => "`[[" + mtch.group(2) + "]]`"
      case "linkplain" => "[[" + mtch.group(2) + "]]"
      case "literal"  => "`" + mtch.group(2) + "`"
      case "value" => "`" + mtch.group(2) + "`"
      case _ => ""
    }
  }

  val safeTagMarker = '\u000E' // IMPORTANT: Only change if you've updated the sanitization tests to match
  val endOfLine     = '\u000A'
  val endOfText     = '\u0003'

  /** A Scaladoc tag not linked to a symbol and not followed by text */
  val SingleTagRegex =
    new Regex("""\s*@(\S+)\s*""")

  /** A Scaladoc tag not linked to a symbol. Returns the name of the tag, and the rest of the line. */
  val SimpleTagRegex =
    new Regex("""\s*@(\S+)\s+(.*)""")

  /** A Scaladoc tag linked to a symbol. Returns the name of the tag, the name
    * of the symbol, and the rest of the line. */
  val SymbolTagRegex =
    new Regex("""\s*@(param|tparam|throws|groupdesc|groupname|groupprio)\s+(\S*)\s*(.*)""")

  /** The start of a Scaladoc code block */
  val CodeBlockStartRegex =
    new Regex("""(.*?)((?:\{\{\{)|(?:```)|(?:\u000E<pre(?: [^>]*)?>\u000E))(.*)""")

  /** The end of a Scaladoc code block */
  val CodeBlockEndRegex =
    new Regex("""(.*?)((?:\}\}\})|(?:```)|(?:\u000E</pre>\u000E))(.*)""")
}
