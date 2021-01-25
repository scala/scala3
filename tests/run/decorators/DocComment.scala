/** Represents a doc comment, splitting it into `body` and `tags`
 *  `tags` are all lines starting with an `@`, where the tag thats starts
 *         with `@` is paired with the text that follows, up to the next
 *         tagged line.
 *  `body` what comes before the first tagged line
 */
case class DocComment(body: String, tags: Map[String, List[String]])
object DocComment:
  def fromString(str: String): DocComment =
    val lines = str.linesIterator.toList
    def tagged(line: String): Option[(String, String)] =
      val ws = WordSplitter(line)
      val tag = ws.next()
      if tag.startsWith("@") then Some(tag, line.drop(ws.nextOffset))
      else None
    val (bodyLines, taggedLines) = lines.span(tagged(_).isEmpty)
    def tagPairs(lines: List[String]): List[(String, String)] = lines match
      case line :: lines1 =>
        val (tag, descPrefix) = tagged(line).get
        val (untaggedLines, lines2) = lines1.span(tagged(_).isEmpty)
        val following = untaggedLines.map(_.dropWhile(_ <= ' '))
        (tag, (descPrefix :: following).mkString("\n")) :: tagPairs(lines2)
      case _ =>
        Nil
    DocComment(bodyLines.mkString("\n"), tagPairs(taggedLines).groupMap(_._1)(_._2))
end DocComment