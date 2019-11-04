package dotty.tools
package dotc
package parsing

import core.Names._, core.Contexts._, core.Decorators._, util.Spans._
import core.StdNames._, core.Comments._
import util.SourceFile
import java.lang.Character.isDigit
import scala.internal.Chars._
import util.NameTransformer.avoidIllegalChars
import util.Spans.Span
import config.Config
import config.Printers.lexical
import config.Settings.Setting
import Tokens._
import scala.annotation.{ switch, tailrec }
import scala.collection.mutable
import scala.collection.immutable.{SortedMap, BitSet}
import rewrites.Rewrites.patch

object Scanners {

  /** Offset into source character array */
  type Offset = Int

  /** An undefined offset */
  val NoOffset: Offset = -1

  type Token = Int

  private val identity: IndentWidth => IndentWidth = Predef.identity

  trait TokenData {

    /** the next token */
    var token: Token = EMPTY

    /** the offset of the first character of the current token */
    var offset: Offset = 0

    /** the offset of the character following the token preceding this one */
    var lastOffset: Offset = 0

    /** the offset of the newline immediately preceding the token, or -1 if
     *  token is not preceded by a newline.
     */
    var lineOffset: Offset = -1

    /** the name of an identifier */
    var name: SimpleName = null

    /** the string value of a literal */
    var strVal: String = null

    /** the base of a number */
    var base: Int = 0

    def copyFrom(td: TokenData): Unit = {
      this.token = td.token
      this.offset = td.offset
      this.lastOffset = td.lastOffset
      this.lineOffset = td.lineOffset
      this.name = td.name
      this.strVal = td.strVal
      this.base = td.base
    }
  }

  abstract class ScannerCommon(source: SourceFile)(implicit ctx: Context) extends CharArrayReader with TokenData {
    val buf: Array[Char] = source.content
    def nextToken(): Unit

    // Errors -----------------------------------------------------------------

    /** the last error offset
      */
    var errOffset: Offset = NoOffset

    /** Generate an error at the given offset */
    def error(msg: String, off: Offset = offset): Unit = {
      errorButContinue(msg, off)
      token = ERROR
      errOffset = off
    }

    def errorButContinue(msg: String, off: Offset = offset): Unit =
      ctx.error(msg, source atSpan Span(off))

    /** signal an error where the input ended in the middle of a token */
    def incompleteInputError(msg: String): Unit = {
      ctx.incompleteInputError(msg, source atSpan Span(offset))
      token = EOF
      errOffset = offset
    }

    // Setting token data ----------------------------------------------------

    /** A character buffer for literals
      */
    protected val litBuf = new mutable.StringBuilder

    /** append Unicode character to "litBuf" buffer
      */
    protected def putChar(c: Char): Unit = litBuf.append(c)

    /** Return buffer contents and clear */
    def flushBuf(buf: StringBuilder): String = {
      val str = buf.toString
      buf.clear()
      str
    }

    /** Clear buffer and set name and token */
    def finishNamed(idtoken: Token = IDENTIFIER, target: TokenData = this): Unit = {
      target.name = termName(flushBuf(litBuf))
      target.token = idtoken
      if (idtoken == IDENTIFIER)
        target.token = toToken(target.name)
    }

    /** The token for given `name`. Either IDENTIFIER or a keyword. */
    def toToken(name: SimpleName): Token

    /** Clear buffer and set string */
    def setStrVal(): Unit =
      strVal = flushBuf(litBuf)

    @inline def isNumberSeparator(c: Char): Boolean = c == '_'

    @inline def removeNumberSeparators(s: String): String =
      if (s.indexOf('_') > 0) s.replaceAllLiterally("_", "") else s

    // disallow trailing numeric separator char, but continue lexing
    def checkNoTrailingSeparator(): Unit =
      if (isNumberSeparator(litBuf.last))
        errorButContinue("trailing separator is not allowed", offset + litBuf.length - 1)
  }

  class Scanner(source: SourceFile, override val startFrom: Offset = 0)(implicit ctx: Context) extends ScannerCommon(source)(ctx) {
    val keepComments = !ctx.settings.YdropComments.value

    /** A switch whether operators at the start of lines can be infix operators */
    private[Scanners] var allowLeadingInfixOperators = true

    val isScala2Mode: Boolean = ctx.scala2Setting

    val rewrite = ctx.settings.rewrite.value.isDefined
    val oldSyntax = ctx.settings.oldSyntax.value
    val newSyntax = ctx.settings.newSyntax.value

    val rewriteToIndent = ctx.settings.indent.value && rewrite
    val rewriteNoIndent = ctx.settings.noindent.value && rewrite

    val noindentSyntax =
      ctx.settings.noindent.value
      || ctx.settings.oldSyntax.value
      || isScala2Mode
    val indentSyntax =
      (if (Config.defaultIndent) !noindentSyntax else ctx.settings.indent.value)
      || rewriteNoIndent
    val colonSyntax =
      ctx.settings.YindentColons.value
      || rewriteNoIndent

    if (rewrite) {
      val s = ctx.settings
      val rewriteTargets = List(s.newSyntax, s.oldSyntax, s.indent, s.noindent)
      val enabled = rewriteTargets.filter(_.value)
      if (enabled.length > 1)
        error(s"illegal combination of -rewrite targets: ${enabled(0).name} and ${enabled(1).name}")
    }

    /** All doc comments kept by their end position in a `Map` */
    private var docstringMap: SortedMap[Int, Comment] = SortedMap.empty

    /* A Buffer for comment positions */
    private val commentPosBuf = new mutable.ListBuffer[Span]

    /** Return a list of all the comment positions */
    def commentSpans: List[Span] = commentPosBuf.toList

    private def addComment(comment: Comment): Unit = {
      val lookahead = lookaheadReader()
      def nextPos: Int = (lookahead.getc(): @switch) match {
        case ' ' | '\t' => nextPos
        case CR | LF | FF =>
          // if we encounter line delimiting whitespace we don't count it, since
          // it seems not to affect positions in source
          nextPos - 1
        case _ => lookahead.charOffset - 1
      }
      docstringMap = docstringMap + (nextPos -> comment)
    }

    /** Returns the closest docstring preceding the position supplied */
    def getDocComment(pos: Int): Option[Comment] = docstringMap.get(pos)

    /** A buffer for comments */
    private val commentBuf = new mutable.StringBuilder

    private def handleMigration(keyword: Token): Token =
      if (!isScala2Mode) keyword
      else if (scala3keywords.contains(keyword)) treatAsIdent()
      else keyword

    private def treatAsIdent() = {
      testScala2Mode(i"$name is now a keyword, write `$name` instead of $name to keep it as an identifier")
      patch(source, Span(offset), "`")
      patch(source, Span(offset + name.length), "`")
      IDENTIFIER
    }

    def toToken(name: SimpleName): Token = {
      val idx = name.start
      if (idx >= 0 && idx <= lastKeywordStart) handleMigration(kwArray(idx))
      else IDENTIFIER
    }

    def newTokenData: TokenData = new TokenData {}

    /** We need one token lookahead and one token history
     */
    val next = newTokenData
    private val prev = newTokenData

    /** The current region. This is initially an Indented region with indentation width. */
    var currentRegion: Region = Indented(IndentWidth.Zero, Set(), EMPTY, null)

    /** The end marker that was skipped last */
    val endMarkers = new mutable.ListBuffer[EndMarker]

// Scala 2 compatibility

    /** Cannot use ctx.featureEnabled because accessing the context would force too much */
    def testScala2Mode(msg: String, span: Span = Span(offset)): Boolean = {
      if (isScala2Mode) ctx.migrationWarning(msg, source.atSpan(span))
      isScala2Mode
    }

    /** A migration warning if in Scala-2 mode, an error otherwise */
    def errorOrMigrationWarning(msg: String, span: Span = Span(offset)): Unit =
      if (isScala2Mode) ctx.migrationWarning(msg, source.atSpan(span))
      else ctx.error(msg, source.atSpan(span))

// Get next token ------------------------------------------------------------

    /** Are we directly in a multiline string interpolation expression?
     *  @pre inStringInterpolation
     */
    private def inMultiLineInterpolation = currentRegion match {
      case InString(multiLine, _) => multiLine
      case _ => false
    }

    /** read next token and return last offset
     */
    def skipToken(): Offset = {
      val off = offset
      nextToken()
      off
    }

    def adjustSepRegions(lastToken: Token): Unit = (lastToken: @switch) match {
      case LPAREN | LBRACKET =>
        currentRegion = InParens(lastToken, currentRegion)
      case LBRACE =>
        currentRegion = InBraces(null, currentRegion)
      case RBRACE =>
        def dropBraces(): Unit = currentRegion match {
          case r: InBraces =>
            currentRegion = r.enclosing
          case _ =>
            if (!currentRegion.isOutermost) {
              currentRegion = currentRegion.enclosing
              dropBraces()
            }
        }
        dropBraces()
      case RPAREN | RBRACKET =>
        currentRegion match {
          case InParens(prefix, outer) if prefix + 1 == lastToken => currentRegion = outer
          case _ =>
        }
      case STRINGLIT =>
        currentRegion match {
          case InString(_, outer) => currentRegion = outer
          case _ =>
        }
      case _ =>
    }

    /** Produce next token, filling TokenData fields of Scanner.
     */
    def nextToken(): Unit = {
      val lastToken = token
      adjustSepRegions(lastToken)

      // Read a token or copy it from `next` tokenData
      if (next.token == EMPTY) {
        lastOffset = lastCharOffset
        currentRegion match {
          case InString(multiLine, _) => fetchStringPart(multiLine)
          case _ => fetchToken()
        }
        if (token == ERROR) adjustSepRegions(STRINGLIT) // make sure we exit enclosing string literal
      }
      else {
        this.copyFrom(next)
        next.token = EMPTY
      }

      if (isAfterLineEnd) handleNewLine(lastToken)
      postProcessToken()
      //printState()
    }

    protected def printState() =
      print("[" + show + "]")

    /** Insert `token` at assumed `offset` in front of current one. */
    def insert(token: Token, offset: Int) = {
      next.copyFrom(this)
      this.offset = offset
      this.token = token
    }

    /** If this token and the next constitute an end marker, skip them and append a new EndMarker
     *  value at the end of the endMarkers queue.
     */
    private def handleEndMarkers(width: IndentWidth): Unit =
      if (next.token == IDENTIFIER && next.name == nme.end && width == currentRegion.indentWidth) {
        val lookahead = LookaheadScanner()
        lookahead.nextToken() // skip the `end`

        def handle(tag: EndMarkerTag) = {
          val skipTo = lookahead.charOffset
          lookahead.nextToken()
          if (lookahead.isAfterLineEnd || lookahead.token == EOF) {
            lexical.println(i"produce end marker $tag $width")
            endMarkers += EndMarker(tag, width, offset)
            next.token = EMPTY
            while (charOffset < skipTo) nextChar()
          }
        }

        lookahead.token match {
          case IDENTIFIER | BACKQUOTED_IDENT => handle(lookahead.name)
          case IF | WHILE | FOR | MATCH | TRY | NEW => handle(lookahead.token)
          case _ =>
        }
      }

    /** Consume and cancel the head of the end markers queue if it has the given `tag` and width.
     *  Flag end markers with higher indent widths as errors.
     */
    def consumeEndMarker(tag: EndMarkerTag, width: IndentWidth): Unit = {
      lexical.println(i"consume end marker $tag $width")
      if (endMarkers.nonEmpty) {
        val em = endMarkers.head
        if (width <= em.width) {
          if (em.tag != tag || em.width != width) {
            lexical.println(i"misaligned end marker ${em.tag}, ${em.width} at ${width}")
            errorButContinue("misaligned end marker", em.offset)
          }
          endMarkers.trimStart(1)
        }
      }
    }

    /** A leading symbolic or backquoted identifier is treated as an infix operator if
      *   - it does not follow a blank line, and
      *   - it is followed on the same line by at least one ' '
      *     and a token that can start an expression.
      *  If a leading infix operator is found and -language:Scala2 or -old-syntax is set,
      *  emit a change warning.
      */
    def isLeadingInfixOperator(inConditional: Boolean = true) = (
      allowLeadingInfixOperators
      && (  token == BACKQUOTED_IDENT
         || token == IDENTIFIER && isOperatorPart(name(name.length - 1)))
      && ch == ' '
      && !pastBlankLine
      && {
        val lookahead = LookaheadScanner()
        lookahead.allowLeadingInfixOperators = false
          // force a NEWLINE a after current token if it is on its own line
        lookahead.nextToken()
        canStartExprTokens.contains(lookahead.token)
      }
      && {
        if isScala2Mode || oldSyntax && !rewrite then
          val (what, previous) =
            if inConditional then ("Rest of line", "previous expression in parentheses")
            else ("Line", "expression on the previous line")
          ctx.warning(em"""$what starts with an operator;
                          |it is now treated as a continuation of the $previous,
                          |not as a separate statement.""",
                      source.atSpan(Span(offset)))
        true
      }
    )

    /** The indentation width of the given offset */
    def indentWidth(offset: Offset): IndentWidth = {
      import IndentWidth.{Run, Conc}
      def recur(idx: Int, ch: Char, n: Int, k: IndentWidth => IndentWidth): IndentWidth =
        if (idx < 0) k(Run(ch, n))
        else {
          val nextChar = buf(idx)
          if (nextChar == LF) k(Run(ch, n))
          else if (nextChar == ' ' || nextChar == '\t')
            if (nextChar == ch)
              recur(idx - 1, ch, n + 1, k)
            else {
              val k1: IndentWidth => IndentWidth = if (n == 0) k else Conc(_, Run(ch, n))
              recur(idx - 1, nextChar, 1, k1)
            }
          else recur(idx - 1, ' ', 0, identity)
        }
      recur(offset - 1, ' ', 0, identity)
    }

    /** Handle newlines, possibly inserting an INDENT, OUTDENT, NEWLINE, or NEWLINES token
     *  in front of the current token. This depends on whether indentation is significant or not.
     *
     *  Indentation is _significant_ if indentSyntax is set, and we are not inside a
     *  {...}, [...], (...), case ... => pair, nor in a if/while condition
     *  (i.e. currentRegion is empty).
     *
     *  There are three rules:
     *
     *   1. Insert NEWLINE or NEWLINES if
     *
     *      - the closest enclosing sepRegion is { ... } or for ... do/yield,
     *         or we are on the toplevel, i.e. currentRegion is empty, and
     *      - the previous token can end a statement, and
     *      - the current token can start a statement, and
     *      - the current token is not a leading infix operator, and
     *      - if indentation is significant then the current token starts at the current
     *        indentation width or to the right of it.
     *
     *      The inserted token is NEWLINES if the current token is preceded by a
     *      whitespace line, or NEWLINE otherwise.
     *
     *   2. Insert INDENT if
     *
     *      - indentation is significant, and
     *      - the last token can start an indentation region.
     *      - the indentation of the current token is strictly greater than the previous
     *        indentation width, or the two widths are the same and the current token is
     *        one of `:` or `match`.
     *
     *      The following tokens can start an indentation region:
     *
     *         :  =  =>  <-  if  then  else  while  do  try  catch  finally  for  yield  match
     *
     *      Inserting an INDENT starts a new indentation region with the indentation of the current
     *      token as indentation width.
     *
     *   3. Insert OUTDENT if
     *
     *      - indentation is significant, and
     *      - the indentation of the current token is strictly less than the
     *        previous indentation width,
     *      - the current token is not a leading infix operator.
     *
     *      Inserting an OUTDENT closes an indentation region. In this case, issue an error if
     *      the indentation of the current token does not match the indentation of some previous
     *      line in an enclosing indentation region.
     *
     *      If a token is inserted and consumed, the original source token is still considered to
     *      start a new line, so the process that inserts an OUTDENT might repeat several times.
     *
     *  Indentation widths are strings consisting of spaces and tabs, ordered by the prefix relation.
     *  I.e. `a <= b` iff `b.startsWith(a)`. If indentation is significant it is considered an error
     *  if the current indentation width and the indentation of the current token are incomparable.
     */
    def handleNewLine(lastToken: Token) = {
      var indentIsSignificant = false
      var newlineIsSeparating = false
      var lastWidth = IndentWidth.Zero
      var indentPrefix = EMPTY
      val nextWidth = indentWidth(offset)
      currentRegion match {
        case r: Indented =>
          indentIsSignificant = indentSyntax
          lastWidth = r.width
          newlineIsSeparating = lastWidth <= nextWidth
          indentPrefix = r.prefix
        case r: InBraces =>
          indentIsSignificant = indentSyntax
          if (r.width == null) r.width = nextWidth
          lastWidth = r.width
          newlineIsSeparating = true
          indentPrefix = LBRACE
        case _ =>
      }
      if newlineIsSeparating
         && canEndStatTokens.contains(lastToken)
         && canStartStatTokens.contains(token)
         && !isLeadingInfixOperator()
      then
        insert(if (pastBlankLine) NEWLINES else NEWLINE, lineOffset)
      else if indentIsSignificant then
        if nextWidth < lastWidth
           || nextWidth == lastWidth && (indentPrefix == MATCH || indentPrefix == CATCH) && token != CASE then
          if !currentRegion.isOutermost &&
             !isLeadingInfixOperator() &&
             !statCtdTokens.contains(lastToken) then
            currentRegion match
              case r: Indented =>
                currentRegion = r.enclosing
                insert(OUTDENT, offset)
                handleEndMarkers(nextWidth)
              case r: InBraces if !closingRegionTokens.contains(token) =>
                ctx.warning("Line is indented too far to the left, or a `}' is missing",
                  source.atSpan(Span(offset)))
              case _ =>

        else if lastWidth < nextWidth
             || lastWidth == nextWidth && (lastToken == MATCH || lastToken == CATCH) && token == CASE then
          if canStartIndentTokens.contains(lastToken) then
            currentRegion = Indented(nextWidth, Set(), lastToken, currentRegion)
            insert(INDENT, offset)
        else if (lastWidth != nextWidth)
          errorButContinue(spaceTabMismatchMsg(lastWidth, nextWidth))
      currentRegion match {
        case Indented(curWidth, others, prefix, outer) if curWidth < nextWidth && !others.contains(nextWidth) =>
          if (token == OUTDENT)
            errorButContinue(
              i"""The start of this line does not match any of the previous indentation widths.
                  |Indentation width of current line : $nextWidth
                  |This falls between previous widths: $curWidth and $lastWidth""")
          else
            currentRegion = Indented(curWidth, others + nextWidth, prefix, outer)
        case _ =>
      }
    }

    def spaceTabMismatchMsg(lastWidth: IndentWidth, nextWidth: IndentWidth) =
      i"""Incompatible combinations of tabs and spaces in indentation prefixes.
         |Previous indent : $lastWidth
         |Latest indent   : $nextWidth"""

    def observeIndented(): Unit =
      if indentSyntax && isNewLine then
        val nextWidth = indentWidth(next.offset)
        val lastWidth = currentRegion match
          case r: IndentSignificantRegion => r.indentWidth
          case _ => nextWidth

        if lastWidth < nextWidth then
          currentRegion = Indented(nextWidth, Set(), COLONEOL, currentRegion)
          offset = next.offset
          token = INDENT
    end observeIndented

    /** Insert an <outdent> token if next token closes an indentation region */
    def observeOutdented(): Unit = currentRegion match
      case r: Indented if !r.isOutermost && closingRegionTokens.contains(token) =>
        currentRegion = r.enclosing
        insert(OUTDENT, offset)
      case _ =>

    /** - Join CASE + CLASS => CASECLASS, CASE + OBJECT => CASEOBJECT, SEMI + ELSE => ELSE, COLON + <EOL> => COLONEOL
     *  - Insert missing OUTDENTs at EOF
     */
    def postProcessToken(): Unit = {
      def lookahead() = {
        prev.copyFrom(this)
        lastOffset = lastCharOffset
        fetchToken()
      }
      def reset() = {
        next.copyFrom(this)
        this.copyFrom(prev)
      }
      def fuse(tok: Int) = {
        token = tok
        offset = prev.offset
        lastOffset = prev.lastOffset
        lineOffset = prev.lineOffset
      }
      token match {
        case CASE =>
          lookahead()
          if (token == CLASS) fuse(CASECLASS)
          else if (token == OBJECT) fuse(CASEOBJECT)
          else reset()
        case SEMI =>
          lookahead()
          if (token != ELSE) reset()
        case COMMA =>
          lookahead()
          if (isAfterLineEnd && (token == RPAREN || token == RBRACKET || token == RBRACE || token == OUTDENT)) {
            /* skip the trailing comma */
          } else if (token == EOF) { // e.g. when the REPL is parsing "val List(x, y, _*,"
            /* skip the trailing comma */
          } else reset()
        case COLON =>
          lookahead()
          val atEOL = isAfterLineEnd
          reset()
          if (colonSyntax && atEOL) token = COLONEOL
        case EOF | RBRACE =>
          currentRegion match {
            case r: Indented if !r.isOutermost =>
              insert(OUTDENT, offset)
              currentRegion = r.outer
            case _ =>
          }
        case _ =>
      }
    }

    /** Is current token first one after a newline? */
    def isAfterLineEnd: Boolean = lineOffset >= 0

    /** Is there a blank line between the current token and the last one?
     *  A blank line consists only of characters <= ' '.
     *  @pre  afterLineEnd().
     */
    private def pastBlankLine: Boolean = {
      val end = offset
      def recur(idx: Offset, isBlank: Boolean): Boolean =
        idx < end && {
          val ch = buf(idx)
          if (ch == LF || ch == FF) isBlank || recur(idx + 1, true)
          else recur(idx + 1, isBlank && ch <= ' ')
        }
      recur(lastOffset, false)
    }

    /** read next token, filling TokenData fields of Scanner.
     */
    protected final def fetchToken(): Unit = {
      offset = charOffset - 1
      lineOffset = if (lastOffset < lineStartOffset) lineStartOffset else -1
      name = null
      (ch: @switch) match {
        case ' ' | '\t' | CR | LF | FF =>
          nextChar()
          fetchToken()
        case 'A' | 'B' | 'C' | 'D' | 'E' |
             'F' | 'G' | 'H' | 'I' | 'J' |
             'K' | 'L' | 'M' | 'N' | 'O' |
             'P' | 'Q' | 'R' | 'S' | 'T' |
             'U' | 'V' | 'W' | 'X' | 'Y' |
             'Z' | '$' | '_' |
             'a' | 'b' | 'c' | 'd' | 'e' |
             'f' | 'g' | 'h' | 'i' | 'j' |
             'k' | 'l' | 'm' | 'n' | 'o' |
             'p' | 'q' | 'r' | 's' | 't' |
             'u' | 'v' | 'w' | 'x' | 'y' |
             'z' =>
          putChar(ch)
          nextChar()
          getIdentRest()
          if (ch == '"' && token == IDENTIFIER)
            token = INTERPOLATIONID
        case '<' => // is XMLSTART?
          def fetchLT() = {
            val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
            nextChar()
            last match {
              case ' ' | '\t' | '\n' | '{' | '(' | '>' if xml.Utility.isNameStart(ch) || ch == '!' || ch == '?' =>
                token = XMLSTART
              case _ =>
                // Console.println("found '<', but last is '" + in.last +"'"); // DEBUG
                putChar('<')
                getOperatorRest()
            }
          }
          fetchLT()
        case '~' | '!' | '@' | '#' | '%' |
             '^' | '*' | '+' | '-' | /*'<' | */
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' =>
          putChar(ch)
          nextChar()
          getOperatorRest()
        case '/' =>
          if (skipComment())
            fetchToken()
          else {
            putChar('/')
            getOperatorRest()
          }
        case '0' =>
          def fetchZero() = {
            putChar(ch)
            nextChar()
            if (ch == 'x' || ch == 'X') {
              nextChar()
              base = 16
              if (isNumberSeparator(ch))
                errorButContinue("leading separator is not allowed", offset + 2)
            }
            else {
              /**
               * What should leading 0 be in the future? It is potentially dangerous
               *  to let it be base-10 because of history.  Should it be an error? Is
               *  there a realistic situation where one would need it?
               */
              if (isDigit(ch) || (isNumberSeparator(ch) && isDigit(lookaheadChar())))
                error("Non-zero numbers may not have a leading zero.")
              base = 10
            }
            getNumber()
          }
          fetchZero()
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          base = 10
          getNumber()
        case '`' =>
          getBackquotedIdent()
        case '\"' =>
          def stringPart(multiLine: Boolean) = {
            getStringPart(multiLine)
            currentRegion = InString(multiLine, currentRegion)
          }
          def fetchDoubleQuote() =
            if (token == INTERPOLATIONID) {
              nextRawChar()
              if (ch == '\"') {
                nextRawChar()
                if (ch == '\"') {
                  nextRawChar()
                  stringPart(multiLine = true)
                }
                else {
                  token = STRINGLIT
                  strVal = ""
                }
              }
              else stringPart(multiLine = false)
            }
            else {
              nextChar()
              if (ch == '\"') {
                nextChar()
                if (ch == '\"') {
                  nextRawChar()
                  getRawStringLit()
                }
                else {
                  token = STRINGLIT
                  strVal = ""
                }
              }
              else
                getStringLit()
            }
          fetchDoubleQuote()
        case '\'' =>
          def fetchSingleQuote() = {
            nextChar()
            if (isIdentifierStart(ch))
              charLitOr { getIdentRest(); QUOTEID }
            else if (isOperatorPart(ch) && (ch != '\\'))
              charLitOr { getOperatorRest(); QUOTEID }
            else ch match {
              case '{' | '[' | ' ' | '\t' if lookaheadChar() != '\'' =>
                token = QUOTE
              case _ =>
                getLitChar()
                if (ch == '\'') finishCharLit()
                else error("unclosed character literal")
            }
          }
          fetchSingleQuote()
        case '.' =>
          nextChar()
          if ('0' <= ch && ch <= '9') {
            putChar('.'); getFraction(); setStrVal()
          }
          else
            token = DOT
        case ';' =>
          nextChar(); token = SEMI
        case ',' =>
          nextChar(); token = COMMA
        case '(' =>
          nextChar(); token = LPAREN
        case '{' =>
          nextChar(); token = LBRACE
        case ')' =>
          nextChar(); token = RPAREN
        case '}' =>
          nextChar(); token = RBRACE
        case '[' =>
          nextChar(); token = LBRACKET
        case ']' =>
          nextChar(); token = RBRACKET
        case SU =>
          if (isAtEnd) token = EOF
          else {
            error("illegal character")
            nextChar()
          }
        case _ =>
          def fetchOther() =
            if (ch == '\u21D2') {
              nextChar(); token = ARROW
            }
            else if (ch == '\u2190') {
              nextChar(); token = LARROW
            }
            else if (Character.isUnicodeIdentifierStart(ch)) {
              putChar(ch)
              nextChar()
              getIdentRest()
            }
            else if (isSpecial(ch)) {
              putChar(ch)
              nextChar()
              getOperatorRest()
            }
            else {
              // FIXME: Dotty deviation: f"" interpolator is not supported (#1814)
              error("illegal character '\\u%04x'".format(ch: Int))
              nextChar()
            }
          fetchOther()
      }
    }

    private def skipComment(): Boolean = {
      def appendToComment(ch: Char) =
        if (keepComments) commentBuf.append(ch)
      def nextChar() = {
        appendToComment(ch)
        Scanner.this.nextChar()
      }
      def skipLine(): Unit = {
        nextChar()
        if ((ch != CR) && (ch != LF) && (ch != SU)) skipLine()
      }
      @tailrec
      def skipComment(): Unit =
        if (ch == '/') {
          nextChar()
          if (ch == '*') nestedComment()
          skipComment()
        }
        else if (ch == '*') {
          while ({ nextChar() ; ch == '*' }) ()
          if (ch == '/') nextChar()
          else skipComment()
        }
        else if (ch == SU) incompleteInputError("unclosed comment")
        else { nextChar(); skipComment() }
      def nestedComment() = { nextChar(); skipComment() }
      val start = lastCharOffset
      def finishComment(): Boolean = {
        if (keepComments) {
          val pos = Span(start, charOffset - 1, start)
          val comment = Comment(pos, flushBuf(commentBuf))
          commentPosBuf += pos

          if (comment.isDocComment)
            addComment(comment)
        }

        true
      }
      nextChar()
      if (ch == '/') { skipLine(); finishComment() }
      else if (ch == '*') { nextChar(); skipComment(); finishComment() }
      else {
        // This was not a comment, remove the `/` from the buffer
        commentBuf.clear()
        false
      }
    }

// Lookahead ---------------------------------------------------------------

    class LookaheadScanner(indent: Boolean = false) extends Scanner(source, offset) {
      override val indentSyntax = indent
      override protected def printState() = {
        print("la:")
        super.printState()
      }
    }

    /** Skip matching pairs of `(...)` or `[...]` parentheses.
     *  @pre  The current token is `(` or `[`
     */
    final def skipParens(multiple: Boolean = true): Unit =
      val opening = token
      nextToken()
      while token != EOF && token != opening + 1 do
        if token == opening && multiple then skipParens() else nextToken()
      nextToken()

    /** Is the token following the current one in `tokens`? */
    def lookaheadIn(tokens: BitSet): Boolean = {
      val lookahead = LookaheadScanner()
      while
        lookahead.nextToken()
        lookahead.isNewLine
      do ()
      tokens.contains(lookahead.token)
    }

    /** Is the current token in a position where a modifier is allowed? */
    def inModifierPosition(): Boolean = {
      val lookahead = LookaheadScanner()
      while
        lookahead.nextToken()
        lookahead.isNewLine || lookahead.isSoftModifier
      do ()
      modifierFollowers.contains(lookahead.token)
    }

// Identifiers ---------------------------------------------------------------

    private def getBackquotedIdent(): Unit = {
      nextChar()
      getLitChars('`')
      if (ch == '`') {
        nextChar()
        finishNamed(BACKQUOTED_IDENT)
        name = avoidIllegalChars(name)
        if (name.length == 0)
          error("empty quoted identifier")
        else if (name == nme.WILDCARD)
          error("wildcard invalid as backquoted identifier")
      }
      else error("unclosed quoted identifier")
    }

    private def getIdentRest(): Unit = (ch: @switch) match {
      case 'A' | 'B' | 'C' | 'D' | 'E' |
           'F' | 'G' | 'H' | 'I' | 'J' |
           'K' | 'L' | 'M' | 'N' | 'O' |
           'P' | 'Q' | 'R' | 'S' | 'T' |
           'U' | 'V' | 'W' | 'X' | 'Y' |
           'Z' | '$' |
           'a' | 'b' | 'c' | 'd' | 'e' |
           'f' | 'g' | 'h' | 'i' | 'j' |
           'k' | 'l' | 'm' | 'n' | 'o' |
           'p' | 'q' | 'r' | 's' | 't' |
           'u' | 'v' | 'w' | 'x' | 'y' |
           'z' |
           '0' | '1' | '2' | '3' | '4' |
           '5' | '6' | '7' | '8' | '9' =>
        putChar(ch)
        nextChar()
        getIdentRest()
      case '_' =>
        putChar(ch)
        nextChar()
        getIdentOrOperatorRest()
      case SU => // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
        finishNamed()
      case _ =>
        if (Character.isUnicodeIdentifierPart(ch)) {
          putChar(ch)
          nextChar()
          getIdentRest()
        }
        else
          finishNamed()
    }

    private def getOperatorRest(): Unit = (ch: @switch) match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' =>
        putChar(ch); nextChar(); getOperatorRest()
      case '/' =>
        if (skipComment()) finishNamed()
        else { putChar('/'); getOperatorRest() }
      case _ =>
        if (isSpecial(ch)) { putChar(ch); nextChar(); getOperatorRest() }
        else finishNamed()
    }

    private def getIdentOrOperatorRest(): Unit =
      if (isIdentifierPart(ch))
        getIdentRest()
      else ch match {
        case '~' | '!' | '@' | '#' | '%' |
             '^' | '*' | '+' | '-' | '<' |
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' | '/' =>
          getOperatorRest()
        case _ =>
          if (isSpecial(ch)) getOperatorRest()
          else finishNamed()
      }

    def isSoftModifier: Boolean =
      token == IDENTIFIER && softModifierNames.contains(name)

    def isSoftModifierInModifierPosition: Boolean =
      isSoftModifier && inModifierPosition()

    def isSoftModifierInParamModifierPosition: Boolean =
      isSoftModifier && !lookaheadIn(BitSet(COLON))

    def isNewLine = token == NEWLINE || token == NEWLINES
    def isIdent = token == IDENTIFIER || token == BACKQUOTED_IDENT

    def isNestedStart = token == LBRACE || token == INDENT
    def isNestedEnd = token == RBRACE || token == OUTDENT

    def canStartStatTokens =
      if isScala2Mode then canStartStatTokens2 else canStartStatTokens3

    def canStartExprTokens =
      if isScala2Mode then canStartExprTokens2 else canStartExprTokens3

// Literals -----------------------------------------------------------------

    private def getStringLit() = {
      getLitChars('"')
      if (ch == '"') {
        setStrVal()
        nextChar()
        token = STRINGLIT
      }
      else error("unclosed string literal")
    }

    private def getRawStringLit(): Unit =
      if (ch == '\"') {
        nextRawChar()
        if (isTripleQuote()) {
          setStrVal()
          token = STRINGLIT
        }
        else
          getRawStringLit()
      }
      else if (ch == SU)
        incompleteInputError("unclosed multi-line string literal")
      else {
        putChar(ch)
        nextRawChar()
        getRawStringLit()
      }

    @annotation.tailrec private def getStringPart(multiLine: Boolean): Unit = {
      def finishStringPart() = {
        setStrVal()
        token = STRINGPART
        next.lastOffset = charOffset - 1
        next.offset = charOffset - 1
      }
      if (ch == '"')
        if (multiLine) {
          nextRawChar()
          if (isTripleQuote()) {
            setStrVal()
            token = STRINGLIT
          }
          else
            getStringPart(multiLine)
        }
        else {
          nextChar()
          setStrVal()
          token = STRINGLIT
        }
      else if (ch == '$') {
        nextRawChar()
        if (ch == '$' || ch == '"') {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        }
        else if (ch == '{') {
          finishStringPart()
          nextRawChar()
          next.token = LBRACE
        }
        else if (Character.isUnicodeIdentifierStart(ch) || ch == '_') {
          finishStringPart()
          while ({
            putChar(ch)
            nextRawChar()
            ch != SU && Character.isUnicodeIdentifierPart(ch)
          })
          ()
          finishNamed(target = next)
        }
        else
          error("invalid string interpolation: `$$', `$\"`, `$'ident or `$'BlockExpr expected")
      }
      else {
        val isUnclosedLiteral = !isUnicodeEscape && (ch == SU || (!multiLine && (ch == CR || ch == LF)))
        if (isUnclosedLiteral)
          if (multiLine)
            incompleteInputError("unclosed multi-line string literal")
          else
            error("unclosed string literal")
        else {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        }
      }
    }

    private def fetchStringPart(multiLine: Boolean) = {
      offset = charOffset - 1
      getStringPart(multiLine)
    }

    private def isTripleQuote(): Boolean =
      if (ch == '"') {
        nextRawChar()
        if (ch == '"') {
          nextChar()
          while (ch == '"') {
            putChar('"')
            nextChar()
          }
          true
        }
        else {
          putChar('"')
          putChar('"')
          false
        }
      }
      else {
        putChar('"')
        false
      }

    /** copy current character into litBuf, interpreting any escape sequences,
     *  and advance to next character.
     */
    protected def getLitChar(): Unit =
      if (ch == '\\') {
        nextChar()
        if ('0' <= ch && ch <= '7') {
          val leadch: Char = ch
          var oct: Int = digit2int(ch, 8)
          nextChar()
          if ('0' <= ch && ch <= '7') {
            oct = oct * 8 + digit2int(ch, 8)
            nextChar()
            if (leadch <= '3' && '0' <= ch && ch <= '7') {
              oct = oct * 8 + digit2int(ch, 8)
              nextChar()
            }
          }
          putChar(oct.toChar)
        }
        else {
          ch match {
            case 'b'  => putChar('\b')
            case 't'  => putChar('\t')
            case 'n'  => putChar('\n')
            case 'f'  => putChar('\f')
            case 'r'  => putChar('\r')
            case '\"' => putChar('\"')
            case '\'' => putChar('\'')
            case '\\' => putChar('\\')
            case _    => invalidEscape()
          }
          nextChar()
        }
      }
      else {
        putChar(ch)
        nextChar()
      }

    protected def invalidEscape(): Unit = {
      error("invalid escape character", charOffset - 1)
      putChar(ch)
    }

    private def getLitChars(delimiter: Char) =
      while (ch != delimiter && !isAtEnd && (ch != SU && ch != CR && ch != LF || isUnicodeEscape))
        getLitChar()

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction(): Unit = {
      token = DECILIT
      while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
        putChar(ch)
        nextChar()
      }
      checkNoTrailingSeparator()
      if (ch == 'e' || ch == 'E') {
        val lookahead = lookaheadReader()
        lookahead.nextChar()
        if (lookahead.ch == '+' || lookahead.ch == '-')
          lookahead.nextChar()
        if ('0' <= lookahead.ch && lookahead.ch <= '9' || isNumberSeparator(ch)) {
          putChar(ch)
          nextChar()
          if (ch == '+' || ch == '-') {
            putChar(ch)
            nextChar()
          }
          while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
            putChar(ch)
            nextChar()
          }
          checkNoTrailingSeparator()
        }
        token = EXPOLIT
      }
      if (ch == 'd' || ch == 'D') {
        putChar(ch)
        nextChar()
        token = DOUBLELIT
      }
      else if (ch == 'f' || ch == 'F') {
        putChar(ch)
        nextChar()
        token = FLOATLIT
      }
      checkNoLetter()
    }
    def checkNoLetter(): Unit =
      if (isIdentifierPart(ch) && ch >= ' ')
        error("Invalid literal number")

    /** Read a number into strVal and set base
    */
    protected def getNumber(): Unit = {
      while (isNumberSeparator(ch) || digit2int(ch, base) >= 0) {
        putChar(ch)
        nextChar()
      }
      checkNoTrailingSeparator()
      token = INTLIT
      if (base == 10 && ch == '.') {
        val lch = lookaheadChar()
        if ('0' <= lch && lch <= '9') {
          putChar('.')
          nextChar()
          getFraction()
        }
      }
      else (ch: @switch) match {
        case 'e' | 'E' | 'f' | 'F' | 'd' | 'D' =>
          if (base == 10) getFraction()
        case 'l' | 'L' =>
          nextChar()
          token = LONGLIT
        case _ =>
      }

      checkNoTrailingSeparator()

      setStrVal()
    }

    private def finishCharLit(): Unit = {
      nextChar()
      token = CHARLIT
      setStrVal()
    }

    /** Parse character literal if current character is followed by \',
     *  or follow with given op and return a symbol literal token
     */
    def charLitOr(op: => Token): Unit = {
      putChar(ch)
      nextChar()
      if (ch == '\'') finishCharLit()
      else {
        token = op
        strVal = if (name != null) name.toString else null
        litBuf.clear()
      }
    }

    override def toString: String =
      showTokenDetailed(token) + {
        if identifierTokens.contains(token) then name
        else if literalTokens.contains(token) then strVal
        else ""
      }

    def show: String = token match {
      case IDENTIFIER | BACKQUOTED_IDENT => s"id($name)"
      case CHARLIT => s"char($strVal)"
      case INTLIT => s"int($strVal, $base)"
      case LONGLIT => s"long($strVal, $base)"
      case FLOATLIT => s"float($strVal)"
      case DOUBLELIT => s"double($strVal)"
      case STRINGLIT => s"string($strVal)"
      case STRINGPART => s"stringpart($strVal)"
      case INTERPOLATIONID => s"interpolationid($name)"
      case SEMI => ";"
      case NEWLINE => ";"
      case NEWLINES => ";;"
      case COMMA => ","
      case _ => showToken(token)
    }

    /* Resume normal scanning after XML */
    def resume(lastTokenData: TokenData): Unit = {
      this.copyFrom(lastTokenData)
      if (next.token != EMPTY && !ctx.reporter.hasErrors)
        error("unexpected end of input: possible missing '}' in XML block")

      nextToken()
    }

   /* Initialization: read first char, then first token */
    nextChar()
    nextToken()
    currentRegion = Indented(indentWidth(offset), Set(), EMPTY, null)
  }
  // end Scanner

  /** A Region indicates what encloses the current token. It can be one of the following
   *
   *   InString    a string interpolation
   *   InParens    a pair of parentheses (...) or brackets [...]
   *   InBraces    a pair of braces { ... }
   *   Indented    a pair of <indent> ... <outdent> tokens
   */
  abstract class Region {
    /** The region enclosing this one, or `null` for the outermost region */
    def outer: Region | Null

    /** Is this region the outermost region? */
    def isOutermost = outer == null

    /** The enclosing region, which is required to exist */
    def enclosing: Region = outer.asInstanceOf[Region]

    /** If this is an InBraces or Indented region, its indentation width, or Zero otherwise */
    def indentWidth: IndentWidth = IndentWidth.Zero
  }

  case class InString(multiLine: Boolean, outer: Region) extends Region
  case class InParens(prefix: Token, outer: Region) extends Region

  abstract class IndentSignificantRegion extends Region

  case class InBraces(var width: IndentWidth | Null, outer: Region)
  extends IndentSignificantRegion {
    override def indentWidth = width
  }

  /** A class describing an indentation region.
   *  @param width   The principal indendation width
   *  @param others  Other indendation widths > width of lines in the same region
   *  @param prefix  The token before the initial <indent> of the region
   */
  case class Indented(width: IndentWidth, others: Set[IndentWidth], prefix: Token, outer: Region | Null)
  extends IndentSignificantRegion {
    override def indentWidth = width
  }

  enum IndentWidth {
    case Run(ch: Char, n: Int)
    case Conc(l: IndentWidth, r: Run)

    def <= (that: IndentWidth): Boolean = this match {
      case Run(ch1, n1) =>
        that match {
          case Run(ch2, n2) => n1 <= n2 && (ch1 == ch2 || n1 == 0)
          case Conc(l, r) => this <= l
        }
      case Conc(l1, r1) =>
        that match {
          case Conc(l2, r2) => l1 == l2 && r1 <= r2
          case _ => false
        }
    }

    def < (that: IndentWidth): Boolean = this <= that && !(that <= this)

    def toPrefix: String = this match {
      case Run(ch, n) => ch.toString * n
      case Conc(l, r) => l.toPrefix ++ r.toPrefix
    }

    override def toString: String = {
      def kind(ch: Char) = ch match {
        case ' ' => "space"
        case '\t' => "tab"
        case _ => s"'$ch'-character"
      }
      this match {
        case Run(ch, n) => s"$n ${kind(ch)}${if (n == 1) "" else "s"}"
        case Conc(l, r) => s"$l, $r"
      }
    }
  }

  object IndentWidth {
    private inline val MaxCached = 40
    private val spaces = Array.tabulate(MaxCached + 1)(new Run(' ', _))
    private val tabs = Array.tabulate(MaxCached + 1)(new Run('\t', _))

    def Run(ch: Char, n: Int): Run =
      if (n <= MaxCached && ch == ' ') spaces(n)
      else if (n <= MaxCached && ch == '\t') tabs(n)
      else new Run(ch, n)

    val Zero = Run(' ', 0)
  }

  /** What can be referred to in an end marker */
  type EndMarkerTag = TermName | Token

  /** A processed end marker
   *  @param tag    The name or token referred to in the marker
   *  @param width  The indentation width where the marker occurred
   *  @param offset The offset of the `end`
   */
  case class EndMarker(tag: EndMarkerTag, width: IndentWidth, offset: Int)

  // ------------- keyword configuration -----------------------------------

  private val (lastKeywordStart, kwArray) = buildKeywordArray(keywords)
}
