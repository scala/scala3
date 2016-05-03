package dotty.tools.dottydoc
package model
package comment

import dotty.tools.dotc.util.Positions._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable
import dotty.tools.dotc.config.Printers.dottydoc
import scala.util.matching.Regex

trait CommentParser extends util.MemberLookup {
  import Regexes._
  import model.internal._

  /** Parses a raw comment string into a `Comment` object.
   * @param packages     all packages parsed by Scaladoc tool, used for lookup
   * @param cleanComment a cleaned comment to be parsed
   * @param src          the raw comment source string.
   * @param pos          the position of the comment in source.
   */
  def parse(
    entity: Entity,
    packages: Map[String, Package],
    comment: List[String],
    src: String,
    pos: Position,
    site: Symbol = NoSymbol
  )(implicit ctx: Context): Body = {

    /** Parses a comment (in the form of a list of lines) to a `Comment`
     *  instance, recursively on lines. To do so, it splits the whole comment
     *  into main body and tag bodies, then runs the `WikiParser` on each body
     *  before creating the comment instance.
     *
     * @param docBody     The body of the comment parsed until now.
     * @param tags        All tags parsed until now.
     * @param lastTagKey  The last parsed tag, or `None` if the tag section
     *                    hasn't started. Lines that are not tagged are part
     *                    of the previous tag or, if none exists, of the body.
     * @param remaining   The lines that must still recursively be parsed.
     * @param inCodeBlock Whether the next line is part of a code block (in
     *                    which no tags must be read).
     */
    def parseComment (
      docBody: StringBuilder,
      tags: Map[TagKey, List[String]],
      lastTagKey: Option[TagKey],
      remaining: List[String],
      inCodeBlock: Boolean
    ): Body = remaining match {

      case CodeBlockStartRegex(before, marker, after) :: ls if (!inCodeBlock) =>
        if (!before.trim.isEmpty && !after.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, before :: marker :: after :: ls, inCodeBlock = false)
        else if (!before.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, before :: marker :: ls, inCodeBlock = false)
        else if (!after.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, marker :: after :: ls, inCodeBlock = true)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            parseComment(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock = true)
          case None =>
            parseComment(docBody append endOfLine append marker, tags, lastTagKey, ls, inCodeBlock = true)
        }

      case CodeBlockEndRegex(before, marker, after) :: ls => {
        if (!before.trim.isEmpty && !after.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, before :: marker :: after :: ls, inCodeBlock = true)
        if (!before.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, before :: marker :: ls, inCodeBlock = true)
        else if (!after.trim.isEmpty)
          parseComment(docBody, tags, lastTagKey, marker :: after :: ls, inCodeBlock = false)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            parseComment(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock = false)
          case None =>
            parseComment(docBody append endOfLine append marker, tags, lastTagKey, ls, inCodeBlock = false)
        }
      }

      case SymbolTagRegex(name, sym, body) :: ls if (!inCodeBlock) => {
        val key = SymbolTagKey(name, sym)
        val value = body :: tags.getOrElse(key, Nil)
        parseComment(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)
      }

      case SimpleTagRegex(name, body) :: ls if (!inCodeBlock) => {
        val key = SimpleTagKey(name)
        val value = body :: tags.getOrElse(key, Nil)
        parseComment(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)
      }

      case SingleTagRegex(name) :: ls if (!inCodeBlock) => {
        val key = SimpleTagKey(name)
        val value = "" :: tags.getOrElse(key, Nil)
        parseComment(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)
      }

      case line :: ls if (lastTagKey.isDefined) => {
        val newtags = if (!line.isEmpty) {
          val key = lastTagKey.get
          val value =
            ((tags get key): @unchecked) match {
              case Some(b :: bs) => (b + endOfLine + line) :: bs
              case None => oops("lastTagKey set when no tag exists for key")
            }
          tags + (key -> value)
        } else tags
        parseComment(docBody, newtags, lastTagKey, ls, inCodeBlock)
      }

      case line :: ls => {
        if (docBody.length > 0) docBody append endOfLine
        docBody append line
        parseComment(docBody, tags, lastTagKey, ls, inCodeBlock)
      }

      case Nil => {
        // Take the {inheritance, content} diagram keys aside, as it doesn't need any parsing
        val inheritDiagramTag = SimpleTagKey("inheritanceDiagram")
        val contentDiagramTag = SimpleTagKey("contentDiagram")

        val inheritDiagramText: List[String] = tags.get(inheritDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val contentDiagramText: List[String] = tags.get(contentDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val stripTags=List(inheritDiagramTag, contentDiagramTag, SimpleTagKey("template"), SimpleTagKey("documentable"))
        val tagsWithoutDiagram = tags.filterNot(pair => stripTags.contains(pair._1))

        val bodyTags: mutable.Map[TagKey, List[Body]] =
          mutable.Map((tagsWithoutDiagram mapValues {tag => tag map (parseWikiAtSymbol(entity, packages, _, pos, site))}).toSeq: _*)

        def oneTag(key: SimpleTagKey, filterEmpty: Boolean = true): Option[Body] =
          ((bodyTags remove key): @unchecked) match {
            case Some(r :: rs) if !(filterEmpty && r.blocks.isEmpty) =>
              if (!rs.isEmpty) dottydoc.println(s"$pos: only one '@${key.name}' tag is allowed")
              Some(r)
            case _ => None
          }

        def allTags(key: SimpleTagKey): List[Body] =
          (bodyTags remove key).getOrElse(Nil).filterNot(_.blocks.isEmpty)

        def allSymsOneTag(key: TagKey, filterEmpty: Boolean = true): Map[String, Body] = {
          val keys: Seq[SymbolTagKey] =
            bodyTags.keys.toSeq flatMap {
              case stk: SymbolTagKey if (stk.name == key.name) => Some(stk)
              case stk: SimpleTagKey if (stk.name == key.name) =>
                dottydoc.println(s"$pos: tag '@${stk.name}' must be followed by a symbol name")
                None
              case _ => None
            }
          val pairs: Seq[(String, Body)] =
            for (key <- keys) yield {
              val bs = (bodyTags remove key).get
              if (bs.length > 1)
                dottydoc.println(s"$pos: only one '@${key.name}' tag for symbol ${key.symbol} is allowed")
              (key.symbol, bs.head)
            }
          Map.empty[String, Body] ++ (if (filterEmpty) pairs.filterNot(_._2.blocks.isEmpty) else pairs)
        }

        def linkedExceptions: Map[String, Body] = {
          val m = allSymsOneTag(SimpleTagKey("throws"), filterEmpty = false)

          m.map { case (targetStr,body) =>
            val link = lookup(entity, packages, targetStr, pos)
            val newBody = body match {
              case Body(List(Paragraph(Chain(content)))) =>
                val descr = Text(" ") +: content
                val entityLink = EntityLink(Monospace(Text(targetStr)), link)
                Body(List(Paragraph(Chain(entityLink +: descr))))
              case _ => body
            }
            (targetStr, newBody)
          }
        }

        // TODO: this method should return a parsed comment with the members below
        //val com = createComment (
        //  body0           = Some(parseWikiAtSymbol(docBody.toString, pos, site)),
        //  authors0        = allTags(SimpleTagKey("author")),
        //  see0            = allTags(SimpleTagKey("see")),
        //  result0         = oneTag(SimpleTagKey("return")),
        //  throws0         = linkedExceptions,
        //  valueParams0    = allSymsOneTag(SimpleTagKey("param")),
        //  typeParams0     = allSymsOneTag(SimpleTagKey("tparam")),
        //  version0        = oneTag(SimpleTagKey("version")),
        //  since0          = oneTag(SimpleTagKey("since")),
        //  todo0           = allTags(SimpleTagKey("todo")),
        //  deprecated0     = oneTag(SimpleTagKey("deprecated"), filterEmpty = false),
        //  note0           = allTags(SimpleTagKey("note")),
        //  example0        = allTags(SimpleTagKey("example")),
        //  constructor0    = oneTag(SimpleTagKey("constructor")),
        //  source0         = Some(clean(src).mkString("\n")),
        //  inheritDiagram0 = inheritDiagramText,
        //  contentDiagram0 = contentDiagramText,
        //  group0          = oneTag(SimpleTagKey("group")),
        //  groupDesc0      = allSymsOneTag(SimpleTagKey("groupdesc")),
        //  groupNames0     = allSymsOneTag(SimpleTagKey("groupname")),
        //  groupPrio0      = allSymsOneTag(SimpleTagKey("groupprio")),
        //  hideImplicitConversions0 = allTags(SimpleTagKey("hideImplicitConversion")),
        //  shortDescription0 = allTags(SimpleTagKey("shortDescription"))
        //)
        //
        //for ((key, _) <- bodyTags)
        //  dottydoc.println(s"$pos: Tag '@${key.name}' is not recognised")

        parseWikiAtSymbol(entity, packages, docBody.toString, pos, site)
      }
    }

    parseComment(new StringBuilder(comment.size), Map.empty, None, comment, inCodeBlock = false)
  }

  /** A key used for a tag map. The key is built from the name of the tag and
    * from the linked symbol if the tag has one.
    * Equality on tag keys is structural. */
  private sealed abstract class TagKey {
    def name: String
  }

  private final case class SimpleTagKey(name: String) extends TagKey
  private final case class SymbolTagKey(name: String, symbol: String) extends TagKey

  /** Something that should not have happened, happened, and Scaladoc should exit. */
  private def oops(msg: String): Nothing =
    throw new IllegalArgumentException("program logic: " + msg)

  /** Parses a string containing wiki syntax into a `Comment` object.
    * Note that the string is assumed to be clean:
    *  - Removed Scaladoc start and end markers.
    *  - Removed start-of-line star and one whitespace afterwards (if present).
    *  - Removed all end-of-line whitespace.
    *  - Only `endOfLine` is used to mark line endings. */
  def parseWikiAtSymbol(
    entity: Entity,
    packages: Map[String, Package],
    string: String,
    pos: Position,
    site: Symbol
  )(implicit ctx: Context): Body = new WikiParser(entity, packages, string, pos, site).document()

  /** Original wikiparser from NSC
    * @author Ingo Maier
    * @author Manohar Jonnalagedda
    * @author Gilles Dubochet
    */
  protected final class WikiParser(
    entity: Entity,
    packages: Map[String, Package],
    val buffer: String,
    pos: Position,
    site: Symbol
  )(implicit ctx: Context) extends CharReader(buffer) { wiki =>
    var summaryParsed = false

    def document(): Body = {
      val blocks = new mutable.ListBuffer[Block]
      while (char != endOfText)
        blocks += block()
      Body(blocks.toList)
    }

    /* BLOCKS */

    /** {{{ block ::= code | title | hrule | listBlock | para }}} */
    def block(): Block = {
      if (checkSkipInitWhitespace("{{{"))
        code()
      else if (checkSkipInitWhitespace('='))
        title()
      else if (checkSkipInitWhitespace("----"))
        hrule()
      else if (checkList)
        listBlock
      else {
        para()
      }
    }

    /** listStyle ::= '-' spc | '1.' spc | 'I.' spc | 'i.' spc | 'A.' spc | 'a.' spc
      * Characters used to build lists and their constructors */
    protected val listStyles = Map[String, (Seq[Block] => Block)]( // TODO Should this be defined at some list companion?
      "- "  -> ( UnorderedList(_) ),
      "1. " -> ( OrderedList(_,"decimal") ),
      "I. " -> ( OrderedList(_,"upperRoman") ),
      "i. " -> ( OrderedList(_,"lowerRoman") ),
      "A. " -> ( OrderedList(_,"upperAlpha") ),
      "a. " -> ( OrderedList(_,"lowerAlpha") )
    )

    /** Checks if the current line is formed with more than one space and one the listStyles */
    def checkList =
      (countWhitespace > 0) && (listStyles.keys exists { checkSkipInitWhitespace(_) })

    /** {{{
      * nListBlock ::= nLine { mListBlock }
      *      nLine ::= nSpc listStyle para '\n'
      * }}}
      * Where n and m stand for the number of spaces. When `m > n`, a new list is nested. */
    def listBlock(): Block = {

      /** Consumes one list item block and returns it, or None if the block is
        * not a list or a different list. */
      def listLine(indent: Int, style: String): Option[Block] =
        if (countWhitespace > indent && checkList)
          Some(listBlock)
        else if (countWhitespace != indent || !checkSkipInitWhitespace(style))
          None
        else {
          jumpWhitespace()
          jump(style)
          val p = Paragraph(inline(isInlineEnd = false))
          blockEnded("end of list line ")
          Some(p)
        }

      /** Consumes all list item blocks (possibly with nested lists) of the
        * same list and returns the list block. */
      def listLevel(indent: Int, style: String): Block = {
        val lines = mutable.ListBuffer.empty[Block]
        var line: Option[Block] = listLine(indent, style)
        while (line.isDefined) {
          lines += line.get
          line = listLine(indent, style)
        }
        val constructor = listStyles(style)
        constructor(lines)
      }

      val indent = countWhitespace
      val style = (listStyles.keys find { checkSkipInitWhitespace(_) }).getOrElse(listStyles.keys.head)
      listLevel(indent, style)
    }

    def code(): Block = {
      jumpWhitespace()
      jump("{{{")
      val str = readUntil("}}}")
      if (char == endOfText)
        reportError(pos, "unclosed code block")
      else
        jump("}}}")
      blockEnded("code block")
      Code(normalizeIndentation(str))
    }

    /** {{{ title ::= ('=' inline '=' | "==" inline "==" | ...) '\n' }}} */
    def title(): Block = {
      jumpWhitespace()
      val inLevel = repeatJump('=')
      val text = inline(check("=" * inLevel))
      val outLevel = repeatJump('=', inLevel)
      if (inLevel != outLevel)
        reportError(pos, "unbalanced or unclosed heading")
      blockEnded("heading")
      Title(text, inLevel)
    }

    /** {{{ hrule ::= "----" { '-' } '\n' }}} */
    def hrule(): Block = {
      jumpWhitespace()
      repeatJump('-')
      blockEnded("horizontal rule")
      HorizontalRule()
    }

    /** {{{ para ::= inline '\n' }}} */
    def para(): Block = {
      val p =
        if (summaryParsed)
          Paragraph(inline(isInlineEnd = false))
        else {
          val s = summary()
          val r =
            if (checkParaEnded()) List(s) else List(s, inline(isInlineEnd = false))
          summaryParsed = true
          Paragraph(Chain(r))
        }
      while (char == endOfLine && char != endOfText)
        nextChar()
      p
    }

    /* INLINES */

    val OPEN_TAG = "^<([A-Za-z]+)( [^>]*)?(/?)>$".r
    val CLOSE_TAG = "^</([A-Za-z]+)>$".r
    private def readHTMLFrom(begin: HtmlTag): String = {
      val list = mutable.ListBuffer.empty[String]
      val stack = mutable.ListBuffer.empty[String]

      begin.close match {
        case Some(HtmlTag(CLOSE_TAG(s))) =>
          stack += s
        case _ =>
          return ""
      }

      do {
        val str = readUntil { char == safeTagMarker || char == endOfText }
        nextChar()

        list += str

        str match {
          case OPEN_TAG(s, _, standalone) => {
            if (standalone != "/") {
              stack += s
            }
          }
          case CLOSE_TAG(s) => {
            if (s == stack.last) {
              stack.remove(stack.length-1)
            }
          }
          case _ => ;
        }
      } while (stack.length > 0 && char != endOfText)

      list mkString ""
    }

    def inline(isInlineEnd: => Boolean): Inline = {

      def inline0(): Inline = {
        if (char == safeTagMarker) {
          val tag = htmlTag()
          HtmlTag(tag.data + readHTMLFrom(tag))
        }
        else if (check("'''")) bold()
        else if (check("''")) italic()
        else if (check("`"))  monospace()
        else if (check("__")) underline()
        else if (check("^"))  superscript()
        else if (check(",,")) subscript()
        else if (check("[[")) link()
        else {
          val str = readUntil {
            char == safeTagMarker ||
            check("''")           ||
            char == '`'           ||
            check("__")           ||
            char == '^'           ||
            check(",,")           ||
            check("[[")           ||
            isInlineEnd           ||
            checkParaEnded        ||
            char == endOfLine
          }
          Text(str)
        }
      }

      val inlines: List[Inline] = {
        val iss = mutable.ListBuffer.empty[Inline]
        iss += inline0()
        while (!isInlineEnd && !checkParaEnded) {
          val skipEndOfLine = if (char == endOfLine) {
            nextChar()
            true
          } else {
            false
          }

          val current = inline0()
          (iss.last, current) match {
            case (Text(t1), Text(t2)) if skipEndOfLine =>
              iss.update(iss.length - 1, Text(t1 + endOfLine + t2))
            case (i1, i2) if skipEndOfLine =>
              iss ++= List(Text(endOfLine.toString), i2)
            case _ => iss += current
          }
        }
        iss.toList
      }

      inlines match {
        case Nil => Text("")
        case i :: Nil => i
        case is => Chain(is)
      }

    }

    def htmlTag(): HtmlTag = {
      jump(safeTagMarker)
      val read = readUntil(safeTagMarker)
      if (char != endOfText) jump(safeTagMarker)
      HtmlTag(read)
    }

    def bold(): Inline = {
      jump("'''")
      val i = inline(check("'''"))
      jump("'''")
      Bold(i)
    }

    def italic(): Inline = {
      jump("''")
      val i = inline(check("''"))
      jump("''")
      Italic(i)
    }

    def monospace(): Inline = {
      jump("`")
      val i = inline(check("`"))
      jump("`")
      Monospace(i)
    }

    def underline(): Inline = {
      jump("__")
      val i = inline(check("__"))
      jump("__")
      Underline(i)
    }

    def superscript(): Inline = {
      jump("^")
      val i = inline(check("^"))
      if (jump("^")) {
        Superscript(i)
      } else {
        Chain(Seq(Text("^"), i))
      }
    }

    def subscript(): Inline = {
      jump(",,")
      val i = inline(check(",,"))
      jump(",,")
      Subscript(i)
    }

    def summary(): Inline = {
      val i = inline(checkSentenceEnded())
      Summary(
        if (jump("."))
          Chain(List(i, Text(".")))
        else
          i
      )
    }

    def link(): Inline = {
      val SchemeUri = """([a-z]+:.*)""".r
      jump("[[")
      val parens = 2 + repeatJump('[')
      val stop  = "]" * parens
      val target = readUntil { check(stop) || isWhitespaceOrNewLine(char) }
      val title =
        if (!check(stop)) Some({
          jumpWhitespaceOrNewLine()
          inline(check(stop))
        })
        else None
      jump(stop)

      (target, title) match {
        case (SchemeUri(uri), optTitle) =>
          Link(uri, optTitle getOrElse Text(uri))
        case (qualName, optTitle) =>
          makeEntityLink(entity, packages, optTitle getOrElse Text(target), pos, target)
      }
    }

    /* UTILITY */

    /** {{{ eol ::= { whitespace } '\n' }}} */
    def blockEnded(blockType: String): Unit = {
      if (char != endOfLine && char != endOfText) {
        reportError(pos, "no additional content on same line after " + blockType)
        jumpUntil(endOfLine)
      }
      while (char == endOfLine)
        nextChar()
    }

    /**
     *  Eliminates the (common) leading spaces in all lines, based on the first line
     *  For indented pieces of code, it reduces the indent to the least whitespace prefix:
     *    {{{
     *       indented example
     *       another indented line
     *       if (condition)
     *         then do something;
     *       ^ this is the least whitespace prefix
     *    }}}
     */
    def normalizeIndentation(_code: String): String = {

      val code = _code.replaceAll("\\s+$", "").dropWhile(_ == '\n') // right-trim + remove all leading '\n'
      val lines = code.split("\n")

      // maxSkip - size of the longest common whitespace prefix of non-empty lines
      val nonEmptyLines = lines.filter(_.trim.nonEmpty)
      val maxSkip = if (nonEmptyLines.isEmpty) 0 else nonEmptyLines.map(line => line.prefixLength(_ == ' ')).min

      // remove common whitespace prefix
      lines.map(line => if (line.trim.nonEmpty) line.substring(maxSkip) else line).mkString("\n")
    }

    def checkParaEnded(): Boolean = {
      (char == endOfText) ||
      ((char == endOfLine) && {
        val poff = offset
        nextChar() // read EOL
        val ok = {
          checkSkipInitWhitespace(endOfLine) ||
          checkSkipInitWhitespace('=') ||
          checkSkipInitWhitespace("{{{") ||
          checkList ||
          checkSkipInitWhitespace('\u003D')
        }
        offset = poff
        ok
      })
    }

    def checkSentenceEnded(): Boolean = {
      (char == '.') && {
        val poff = offset
        nextChar() // read '.'
        val ok = char == endOfText || char == endOfLine || isWhitespace(char)
        offset = poff
        ok
      }
    }

    def reportError(pos: Position, message: String) =
      dottydoc.println(s"$pos: $message")
  }

  protected sealed class CharReader(buffer: String) { reader =>

    var offset: Int = 0
    def char: Char =
      if (offset >= buffer.length) endOfText else buffer charAt offset

    final def nextChar() =
      offset += 1

    final def check(chars: String): Boolean = {
      val poff = offset
      val ok = jump(chars)
      offset = poff
      ok
    }

    def checkSkipInitWhitespace(c: Char): Boolean = {
      val poff = offset
      jumpWhitespace()
      val ok = jump(c)
      offset = poff
      ok
    }

    def checkSkipInitWhitespace(chars: String): Boolean = {
      val poff = offset
      jumpWhitespace()
      val (ok0, chars0) =
        if (chars.charAt(0) == ' ')
          (offset > poff, chars substring 1)
        else
          (true, chars)
      val ok = ok0 && jump(chars0)
      offset = poff
      ok
    }

    def countWhitespace: Int = {
      var count = 0
      val poff = offset
      while (isWhitespace(char) && char != endOfText) {
        nextChar()
        count += 1
      }
      offset = poff
      count
    }

    /* Jumpers */

    /** Jumps a character and consumes it
      * @return true only if the correct character has been jumped */
    final def jump(ch: Char): Boolean = {
      if (char == ch) {
        nextChar()
        true
      }
      else false
    }

    /** Jumps all the characters in chars, consuming them in the process.
      * @return true only if the correct characters have been jumped
      */
    final def jump(chars: String): Boolean = {
      var index = 0
      while (index < chars.length && char == chars.charAt(index) && char != endOfText) {
        nextChar()
        index += 1
      }
      index == chars.length
    }

    final def repeatJump(c: Char, max: Int = Int.MaxValue): Int = {
      var count = 0
      while (jump(c) && count < max)
        count += 1
      count
    }

    final def jumpUntil(ch: Char): Int = {
      var count = 0
      while (char != ch && char != endOfText) {
        nextChar()
        count += 1
      }
      count
    }

    final def jumpUntil(pred: => Boolean): Int = {
      var count = 0
      while (!pred && char != endOfText) {
        nextChar()
        count += 1
      }
      count
    }

    def jumpWhitespace() = jumpUntil(!isWhitespace(char))

    def jumpWhitespaceOrNewLine() = jumpUntil(!isWhitespaceOrNewLine(char))


    /* Readers */
    final def readUntil(c: Char): String = {
      withRead {
        while (char != c && char != endOfText) {
          nextChar()
        }
      }
    }

    final def readUntil(chars: String): String = {
      assert(chars.length > 0)
      withRead {
        val c = chars.charAt(0)
        while (!check(chars) && char != endOfText) {
          nextChar()
          while (char != c && char != endOfText)
            nextChar()
        }
      }
    }

    final def readUntil(pred: => Boolean): String = {
      withRead {
        while (char != endOfText && !pred) {
          nextChar()
        }
      }
    }

    private def withRead(read: => Unit): String = {
      val start = offset
      read
      buffer.substring(start, offset)
    }

    /* Chars classes */
    def isWhitespace(c: Char) = c == ' ' || c == '\t'

    def isWhitespaceOrNewLine(c: Char) = isWhitespace(c) || c == '\n'
  }
}
