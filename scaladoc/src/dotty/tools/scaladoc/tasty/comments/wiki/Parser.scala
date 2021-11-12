package dotty.tools.scaladoc
package tasty.comments.wiki

import scala.collection.mutable
import scala.annotation.tailrec

import dotty.tools.scaladoc.tasty.comments.Regexes._

/** Original wikiparser from NSC
  * @author Ingo Maier
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet
  */
final class Parser(
  val buffer: String,
  linkResolver: (String, Option[Inline]) => Inline
) extends CharReader(buffer) { wiki =>
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
      listBlock()
    else if (checkTableRow)
      table()
    else {
      para()
    }
  }

  /** listStyle ::= '-' spc | '1.' spc | 'I.' spc | 'i.' spc | 'A.' spc | 'a.' spc
    * Characters used to build lists and their constructors */
  protected val listStyles = Map[String, (Seq[Block] => Block)](
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
        Some(listBlock())
      else if (countWhitespace != indent || !checkSkipInitWhitespace(style))
        None
      else {
        jumpWhitespace()
        jump(style)
        val p = Paragraph(getInline(isInlineEnd = false))
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
      constructor(lines.toList)
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
      reportError("unclosed code block")
    else
      jump("}}}")
    blockEnded("code block")
    Code(normalizeIndentation(str))
  }

  /** {{{ title ::= ('=' inline '=' | "==" inline "==" | ...) '\n' }}} */
  def title(): Block = {
    jumpWhitespace()
    val inLevel = repeatJump('=')
    val text = getInline(check("=" * inLevel))
    val outLevel = repeatJump('=', inLevel)
    if (inLevel != outLevel)
      reportError("unbalanced or unclosed heading")
    blockEnded("heading")
    Title(text, inLevel)
  }

  /** {{{ hrule ::= "----" { '-' } '\n' }}} */
  def hrule(): Block = {
    jumpWhitespace()
    repeatJump('-')
    blockEnded("horizontal rule")
    HorizontalRule
  }


    /** Starts and end with a cell separator matching the minimal row || and all other possible rows */
    private val TableRow = """^\|.*\|$""".r
    private val TableCellStart = "|"

    /* Checks for a well-formed table row */
    private def checkTableRow = {
      check(TableCellStart) && {
        val newlineIdx = buffer.indexOf('\n', offset)
        newlineIdx != -1 &&
          TableRow.findFirstIn(buffer.substring(offset, newlineIdx)).isDefined
      }
    }

  /** {{{
    * table         ::= headerRow '\n' delimiterRow '\n' dataRows '\n'
    * content       ::= inline-content
    * row           ::= '|' { content '|' }+
    * headerRow     ::= row
    * dataRows      ::= row*
    * align         ::= ':' '-'+ | '-'+ | '-'+ ':' | ':' '-'+ ':'
    * delimiterRow :: = '|' { align '|' }+
    * }}}
    */
  def table(): Block = {
    /* Helpers */

    def peek(tag: String): Unit = {
      val peek: String = buffer.substring(offset)
      val limit = 60
      val limitedPeek = peek.substring(0, limit min peek.length)
      println(s"peek: $tag: '$limitedPeek'")
    }

    /* Accumulated state */

    var header: Option[Row] = None

    val rows = mutable.ListBuffer.empty[Row]

    val cells = mutable.ListBuffer.empty[Cell]

    def finalizeCells(): Unit = {
      if (cells.nonEmpty) {
        rows += Row(cells.toList)
      }
      cells.clear()
    }

    def finalizeHeaderCells(): Unit = {
      if (cells.nonEmpty) {
        if (header.isDefined) {
          reportError("more than one table header")
        } else {
          header = Some(Row(cells.toList))
        }
      }
      cells.clear()
    }

    val escapeChar = "\\"

    /* Poor man's negative lookbehind */
    def checkInlineEnd =
      (check(TableCellStart) && !check(escapeChar, -1)) || check("\n")

    def decodeEscapedCellMark(text: String) = text.replace(escapeChar + TableCellStart, TableCellStart)

    def isEndOfText = char == endOfText

    //def isNewline = char == endOfLine

    //def skipNewline() = jump(endOfLine)

    def isStartMarkNewline = check(TableCellStart + endOfLine)

    def skipStartMarkNewline() = jump(TableCellStart + endOfLine)

    def isStartMark = check(TableCellStart)

    def skipStartMark() = jump(TableCellStart)

    def contentNonEmpty(content: Inline) = content != Text("")

      /* Parse cells of a table.
      * @param cellStartMark The char indicating the start or end of a cell
      * @param finalizeRow   Function to invoke when the row has been fully parsed
      */
    def parseCells(cellStartMark: String, finalizeRow: () => Unit): Unit = {
      def jumpCellStartMark() = {
        if (!jump(cellStartMark)) {
          peek(s"Expected $cellStartMark")
          sys.error(s"Precondition violated: Expected $cellStartMark.")
        }
      }

      val startPos = offset

      jumpCellStartMark()

      val content = Paragraph(getInline(isInlineEnd = checkInlineEnd, textTransform = decodeEscapedCellMark))

      parseCells0(content :: Nil, finalizeRow, startPos, offset)
    }

    // Continue parsing a table row.
    //
    // After reading inline content the following conditions will be encountered,
    //
    //    Case : Next Chars
    //    ..................
    //      1  : end-of-text
    //      2  : '|' '\n'
    //      3  : '|'
    //      4  : '\n'
    //
    // Case 1.
    // State : End of text
    // Action: Store the current contents, close the row, report warning, stop parsing.
    //
    // Case 2.
    // State : The cell separator followed by a newline
    // Action: Store the current contents, skip the cell separator and newline, close the row, stop parsing.
    //
    // Case 3.
    // State : The cell separator not followed by a newline
    // Action: Store the current contents, skip the cell separator, continue parsing the row.
    //
    @tailrec def parseCells0(
                              contents: List[Block],
                              finalizeRow: () => Unit,
                              progressPreParse: Int,
                              progressPostParse: Int
                            ): Unit = {

      def storeContents() = cells += Cell(contents.reverse)

      val startPos = offset

      // The ordering of the checks ensures the state checks are correct.
      if (progressPreParse == progressPostParse) {
        peek("no-progress-table-row-parsing")
        sys.error("No progress while parsing table row")
      } else if (isEndOfText) {
        // peek("1: end-of-text")
        // Case 1
        storeContents()
        finalizeRow()
        reportError("unclosed table row")
      } else if (isStartMarkNewline) {
        // peek("2: start-mark-new-line/before")
        // Case 2
        storeContents()
        finalizeRow()
        skipStartMarkNewline()
        // peek("2: start-mark-new-line/after")
      } else if (isStartMark) {
        // peek("3: start-mark")
        // Case 3
        storeContents()
        skipStartMark()
        val content = getInline(isInlineEnd = checkInlineEnd, textTransform = decodeEscapedCellMark)
        // TrailingCellsEmpty produces empty content
        val accContents = if (contentNonEmpty(content)) Paragraph(content) :: Nil else Nil
        parseCells0(accContents, finalizeRow, startPos, offset)
      } else {
        // Case π√ⅈ
        // When the impossible happens leave some clues.
        reportError("unexpected table row markdown")
        peek("parseCell0")
        storeContents()
        finalizeRow()
      }
    }

    /* Parsing */

    jumpWhitespace()

    parseCells(TableCellStart, () => finalizeHeaderCells())

    while (checkTableRow) {
      val initialOffset = offset

      parseCells(TableCellStart, () => finalizeCells())

      /* Progress should always be made */
      if (offset == initialOffset) {
        peek("no-progress-table-parsing")
        sys.error("No progress while parsing table")
      }
    }

    /* Finalize */

    /* Structural consistency checks and coercion */

    // https://github.github.com/gfm/#tables-extension-
    // TODO: The header row must match the delimiter row in the number of cells. If not, a table will not be recognized:
    // TODO: Break at following block level element: The table is broken at the first empty line, or beginning of another block-level structure:
    // TODO: Do not return a table when: The header row must match the delimiter row in the number of cells. If not, a table will not be recognized

    if (cells.nonEmpty) {
      reportError(s"Parsed and unused content: $cells")
    }
    assert(header.isDefined, "table header was not parsed")
    val enforcedCellCount = header.get.cells.size

    def applyColumnCountConstraint(row: Row, defaultCell: Cell, rowType: String): Row = {
      if (row.cells.size == enforcedCellCount)
        row
      else if (row.cells.size > enforcedCellCount) {
        val excess = row.cells.size - enforcedCellCount
        reportError(s"Dropping $excess excess table $rowType cells from row.")
        Row(row.cells.take(enforcedCellCount))
      } else {
        val missing = enforcedCellCount - row.cells.size
        Row(row.cells ++ List.fill(missing)(defaultCell))
      }
    }

    // TODO: Abandon table parsing when the delimiter is missing instead of fixing and continuing.
    val delimiterRow :: dataRows = {
      if (rows.nonEmpty) rows.toList
      else {
        reportError("Fixing missing delimiter row")
        Row(Cell(Paragraph(Text("-")) :: Nil) :: Nil) :: Nil
      }
    }

    if (delimiterRow.cells.isEmpty) sys.error("TODO: Handle table with empty delimiter row")

    val constrainedDelimiterRow = applyColumnCountConstraint(delimiterRow, delimiterRow.cells(0), "delimiter")

    val constrainedDataRows = dataRows.map(applyColumnCountConstraint(_, Cell(Nil), "data"))

    /* Convert the row following the header row to column options */

    val leftAlignmentPattern = "^:?-++$".r
    val centerAlignmentPattern = "^:-++:$".r
    val rightAlignmentPattern = "^-++:$".r

    import ColumnOption._
    /* Encourage user to fix by defaulting to least ignorable fix. */
    val defaultColumnOption = ColumnOptionRight
    val columnOptions = constrainedDelimiterRow.cells.map {
      alignmentSpecifier =>
        alignmentSpecifier.blocks match {
          // TODO: Parse the second row without parsing inline markdown
          // TODO: Save pos when delimiter row is parsed and use here in reported errors
          case Paragraph(Text(as)) :: Nil =>
            as.trim match {
              case leftAlignmentPattern(_*) => ColumnOptionLeft
              case centerAlignmentPattern(_*) => ColumnOptionCenter
              case rightAlignmentPattern(_*) => ColumnOptionRight
              case x =>
                reportError(s"Fixing invalid column alignment: $x")
                defaultColumnOption
            }
          case x =>
            reportError(s"Fixing invalid column alignment: $x")
            defaultColumnOption
        }
    }

    if (check("\n", -1)) {
      prevChar()
    } else {
      peek("expected-newline-missing")
      sys.error("table parsing left buffer in unexpected state")
    }

    blockEnded("table")
    Table(header.get, columnOptions, constrainedDataRows)
  }

  /** {{{ para ::= inline '\n' }}} */
  def para(): Block = {
    val p =
      if (summaryParsed)
        Paragraph(getInline(isInlineEnd = false))
      else {
        val s = summary()
        val r =
          if (checkParaEnded()) List(s) else List(s, getInline(isInlineEnd = false))
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

    while ({
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

      stack.length > 0 && char != endOfText
    }) do {}

    list mkString
  }

  def getInline(isInlineEnd: => Boolean, textTransform: String => String = identity): Inline = {

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
              check("''") ||
              char == '`' ||
              check("__") ||
              char == '^' ||
              check(",,") ||
              check("[[") ||
              isInlineEnd ||
              checkParaEnded() ||
              char == endOfLine
          }
          Text(textTransform(str))
        }
      }

      val inlines: List[Inline] = {
        val iss = mutable.ListBuffer.empty[Inline]
        iss += inline0()
        while (!isInlineEnd && !checkParaEnded()) {
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
    val i = getInline(check("'''"))
    jump("'''")
    Bold(i)
  }

  def italic(): Inline = {
    jump("''")
    val i = getInline(check("''"))
    jump("''")
    Italic(i)
  }

  def monospace(): Inline = {
    jump("`")
    val i = getInline(check("`"))
    jump("`")
    Monospace(i)
  }

  def underline(): Inline = {
    jump("__")
    val i = getInline(check("__"))
    jump("__")
    Underline(i)
  }

  def superscript(): Inline = {
    jump("^")
    val i = getInline(check("^"))
    if (jump("^")) {
      Superscript(i)
    } else {
      Chain(Seq(Text("^"), i))
    }
  }

  def subscript(): Inline = {
    jump(",,")
    val i = getInline(check(",,"))
    jump(",,")
    Subscript(i)
  }

  def summary(): Inline = {
    val i = getInline(checkSentenceEnded())
    Summary(
      if (jump("."))
        Chain(List(i, Text(".")))
      else
        i
    )
  }

  def link(): Inline = {
    jump("[[")
    val parens = 2 + repeatJump('[')
    val stop  = "]" * parens
    val target = readUntil { check(stop) || isWhitespaceOrNewLine(char) }
    val title =
      if (!check(stop)) Some({
        jumpWhitespaceOrNewLine()
        getInline(check(stop))
      })
      else None
    jump(stop)

    linkResolver(target, title)
  }

  /* UTILITY */

  /** {{{ eol ::= { whitespace } '\n' }}} */
  def blockEnded(blockType: String): Unit = {
    if (char != endOfLine && char != endOfText) {
      reportError("no additional content on same line after " + blockType)
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
    val maxSkip = if (nonEmptyLines.isEmpty) 0 else nonEmptyLines.map(line => line.iterator.takeWhile(_ == ' ').size).min

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
        check(TableCellStart) ||
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

  def reportError(message: String) = println(s"$message")
}

sealed class CharReader(buffer: String) { reader =>

  var offset: Int = 0
  def char: Char =
    if (offset >= buffer.length) endOfText else buffer charAt offset

  final def nextChar() =
    offset += 1

  final def prevChar(): Unit =  {
    offset -= 1
  }

  final def check(chars: String): Boolean = {
    val poff = offset
    val ok = jump(chars)
    offset = poff
    ok
  }

  final def check(chars: String, checkOffset: Int): Boolean = {
    val poff = offset
    offset += checkOffset
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
