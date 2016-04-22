package dotty.tools
package dotc
package repl

import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList._
import ammonite.terminal.SpecialKeys._
import ammonite.terminal.Filter
import ammonite.terminal._
import scala.annotation.switch
import scala.collection.mutable.StringBuilder

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {
  private def none(str: String)       = str
  private def keyword(str: String)    = Console.CYAN + str + Console.RESET
  private def typeDef(str: String)    = Console.GREEN + str + Console.RESET
  private def literal(str: String)    = Console.MAGENTA + str + Console.RESET
  private def annotation(str: String) = Console.RED + str + Console.RESET

  private val keywords =
    "abstract"  :: "class"    :: "case"      :: "catch"     :: "def"      ::
    "do"        :: "extends"  :: "else"      :: "false"     :: "finally"  ::
    "final"     :: "for"      :: "forSome"   :: "if"        :: "import"   ::
    "implicit"  :: "lazy"     :: "match"     :: "null"      :: "new"      ::
    "object"    :: "override" :: "private"   :: "protected" :: "package"  ::
    "return"    :: "sealed"   :: "super"     :: "true"      :: "trait"    ::
    "type"      :: "try"      :: "this"      :: "throw"     :: "val"      ::
    "var"       :: "with"     :: "while"     :: "yield"     :: Nil

  private val interpolationPrefixes =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' ::
    'K' :: 'L' :: 'M' :: 'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' ::
    'U' :: 'V' :: 'W' :: 'X' :: 'Y' :: 'Z' :: '$' :: '_' :: 'a' :: 'b' ::
    'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' ::
    'm' :: 'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' ::
    'w' :: 'x' :: 'y' :: 'z' :: Nil

  private val typeEnders =
   '{' :: '}' :: ')' :: '(' :: '=' :: ' ' :: ',' :: '.' :: Nil

  def apply(buffer: Iterable[Char]): Vector[Char] = {
    var prev: Char = 0
    var iter       = buffer.toIterator
    val newBuf     = new StringBuilder

    @inline def keywordStart =
      prev == 0 || prev == ' ' || prev == '{' || prev == '('

    @inline def numberStart(c: Char) =
      c.isDigit && (!prev.isLetter || prev == '.' || prev == ' ' || prev == '(' || prev == '\u0000')

    while(iter.hasNext) {
      val n = iter.next
      if (interpolationPrefixes.contains(n)) {
        // Interpolation prefixes are a superset of the keyword start chars
        val next = iter.take(3).mkString
        if (next.startsWith("\"")) {
          newBuf += n
          prev = n
          appendLiteral('"', next.toIterator.drop(1), next == "\"\"\"")
        } else {
          val (dup, _ ) = iter.duplicate
          iter = next.toIterator ++ dup
          if (n.isUpper && keywordStart) {
            appendWhile(n, !typeEnders.contains(_), typeDef)
          } else if (keywordStart) {
            append(n, keywords.contains(_), keyword)
          } else {
            newBuf += n
            prev = n
          }
        }
      } else {
        (n: @switch) match {
          case '='  =>
            append('=', _ == "=>", keyword)
          case '<'  =>
            append('<', { x => x == "<-" || x == "<:" || x == "<%" }, keyword)
          case '>'  =>
            append('>', { x => x == ">:" }, keyword)
          case '#'  =>
            newBuf append keyword("#")
            prev = '#'
          case '@'  =>
            appendWhile('@', _ != ' ', annotation)
          case '\"' => iter.take(2).mkString match {
            case "\"\"" => appendLiteral('\"', Iterator.empty, multiline = true)
            case lit    => appendLiteral('\"', lit.toIterator)
          }
          case '\'' =>
            appendLiteral('\'', Iterator.empty)
          case '`'  =>
            appendUntil('`', _ == '`', none)
          case c if c.isUpper && keywordStart =>
            appendWhile(c, !typeEnders.contains(_), typeDef)
          case c if numberStart(c) =>
            appendWhile(c, { x => x.isDigit || x == '.' || x == '\u0000'}, literal)
          case c =>
            newBuf += c; prev = c
        }
      }
    }

    def appendLiteral(delim: Char, nextTwo: Iterator[Char], multiline: Boolean = false) = {
      var curr: Char      = 0
      var continue        = true
      var closing         = 0
      val inInterpolation = interpolationPrefixes.contains(prev)
      newBuf append (Console.MAGENTA + delim)


      while (continue && (iter.hasNext || nextTwo.hasNext)) {
        curr = if(nextTwo.hasNext) nextTwo.next else iter.next
        if (curr == '\\' && (iter.hasNext || nextTwo.hasNext)) {
          val next = if (nextTwo.hasNext) nextTwo.next else iter.next
          newBuf append (Console.CYAN + curr)
          if (next == 'u') {
            val code = "u" + iter.take(4).mkString
            newBuf append code
          } else newBuf += next
          newBuf append Console.MAGENTA
          closing = 0
        } else if (inInterpolation && curr == '$' && prev != '$' && (iter.hasNext || nextTwo.hasNext)) { //TODO - break me out!
          val next: Char = if (nextTwo.hasNext) nextTwo.next else iter.next
          if (next == '$') {
            newBuf += curr
            newBuf += next
            prev = '$'
          } else if (next == '{') {
            newBuf append (Console.CYAN + curr)
            newBuf += next
            if (iter.hasNext) {
              var c = iter.next
              while (iter.hasNext && c != '}') {
                newBuf += c
                c = iter.next
              }
              newBuf += c
              newBuf append Console.MAGENTA
            }
          } else { //TODO - break me out
            newBuf append (Console.CYAN + curr)
            newBuf += next
            var c: Char = 'a'
            while (c.isLetterOrDigit && (iter.hasNext || nextTwo.hasNext)) {
              c = if (nextTwo.hasNext) nextTwo.next else iter.next
              if (c != '"') newBuf += c
            }
            newBuf append Console.MAGENTA
            if (c == '"') newBuf += c
          }
          closing = 0
        } else if (curr == delim && multiline) {
          closing += 1
          if (closing == 3) continue = false
          newBuf += curr
        } else if (curr == delim) {
          continue = false
          newBuf += curr
        } else {
          newBuf += curr
          closing = 0
        }
      }
      newBuf append Console.RESET
      prev = curr
    }

    def append(c: Char, shouldHL: String => Boolean, highlight: String => String, pre: Iterator[Char] = Iterator.empty) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while ((pre.hasNext || iter.hasNext) && curr != ' ' && curr != '(') {
        curr = if (pre.hasNext) pre.next else iter.next
        if (curr != ' ') sb += curr
      }

      val str    = sb.toString
      val toAdd  = if (shouldHL(str)) highlight(str) else str
      val suffix = if (curr == ' ') " " else ""
      newBuf append (toAdd + suffix)
      prev = curr
    }

    def appendWhile(c: Char, pred: Char => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while (iter.hasNext && pred(curr)) {
        curr = iter.next
        if (pred(curr)) sb += curr
      }

      val str    = sb.toString
      val suffix = if (!pred(curr)) s"$curr" else ""
      newBuf append (highlight(str) + suffix)
      prev = curr
    }

    def appendUntil(c: Char, pred: Char => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while (iter.hasNext && !pred(curr)) {
        curr = iter.next
        sb += curr
      }

      newBuf append (highlight(sb.toString))
      prev = curr
    }

    newBuf.toVector
  }

  import util.SourceFile
  import parsing.Scanners.Scanner
  import dotc.core.Contexts._
  import dotc.parsing.Tokens._
  import reporting._
  def apply(source: SourceFile)(implicit ctx: Context): Vector[Char] = {
    val freshCtx = ctx.fresh.setReporter(new Reporter {
      def doReport(d: Diagnostic)(implicit ctx: Context) = ()
    })
    val scanner  = new Scanner(source, preserveWhitespace = true)(freshCtx)
    val buf      = new StringBuilder()
    var prev     = List(EMPTY)

    var realLength = 0
    /** `accept` is used to allow for `realLength` to be used to infer "magically
     *  missing tokens"
     */
    def accept(str: String): String = {
      realLength += str.length
      str
    }

    while (scanner.token != EOF) {
      buf append (scanner.token match {
        // Whitespace
        case WHITESPACE => accept({
          if (prev.head == ERROR) scanner.litBuf.toString
          else ""
        } + scanner.strVal)

        // Identifiers
        case BACKQUOTED_IDENT =>
          accept(s"""`${scanner.name.show}`""")
        case id if identifierTokens contains id => {
          val name = accept(scanner.name.show)
          if (name.head.isUpper) typeDef(name) else name
        }

        // Literals
        case INTERPOLATIONID =>
          parseInterpStr()
        case STRINGLIT =>
          literal(accept(s""""${scanner.strVal}""""))
        case CHARLIT =>
          literal(accept(s"""'${scanner.strVal}'"""))
        case SYMBOLLIT =>
          accept("'" + scanner.strVal)
        case lit if literalTokens contains lit =>
          literal(accept(scanner.strVal))

        // Unclosed literals caught using startedLiteral var
        case ERROR =>
          val start = scanner.startedLiteral
          accept(if (start != null) start else "")

        // Keywords
        case EQUALS | COLON => accept(scanner.name.show)
        case k if alphaKeywords.contains(k) || symbolicKeywords.contains(k) =>
          keyword(accept(scanner.name.show))

        // Other minor tokens (i.e. '{' etc)
        case EMPTY => ""
        case XMLSTART => accept("<")
        case _ => accept(tokenString(scanner.token).replaceAll("\'", ""))
      })
      prev = scanner.token :: prev
      scanner.nextToken()
    }

    def parseInterpStr(): String = {
      // print InterpolationID 's' etc
      val sb = new StringBuilder
      sb append accept(scanner.name.show)
      prev = scanner.token :: prev
      scanner.nextToken()

      /**
       * The composition of interpolated strings:
       * s"hello $guy!"
       * ^ ^^^^^^  ^ ^
       * |   |     | |
       * |   |     | |
       * |   |     | STRINGLIT
       * |   |     IDENTIFIER
       * |   STRINGPART
       * INTERPOLATIONID
       *
       * As such, get tokens until EOF or STRINGLIT is encountered
       */
      def scan() = scanner.token match {
        case STRINGPART => {
          val delim =
            if (scanner.inMultiLineInterpolation) "\"\"\""
            else "\""

          if (prev.head == INTERPOLATIONID) literal(accept(delim))
          else ""
        } + literal(accept(scanner.strVal))

        case id if identifierTokens contains id => {
          val name = scanner.name.show
          // $ symbols are not caught by the scanner, infer them
          val prefix = if (prev.head == STRINGPART) "$" else ""
          accept(prefix + name)
        }
        case WHITESPACE => accept({
          // Whitespace occurs in interpolated strings where there
          // is an error - e.g. unclosed string literal
          //
          // Or in blocks i.e. ${...WHITESPACE...}
          if (prev.head == ERROR) scanner.litBuf.toString
          else ""
        } + scanner.strVal)
        case CHARLIT =>
          literal(accept(s"""'${scanner.strVal}'"""))
        case SYMBOLLIT =>
          accept("'" + scanner.strVal)
        case lit if literalTokens contains lit  =>
          literal(accept(scanner.strVal))
        case LBRACE =>
          // An `LBRACE` will only occur if it precedes a block, ergo we can
          // infer "${"
          accept("${")
        case RBRACE => accept("}")
        case ERROR => {
          // FIXME: the behaviour here is weird, the check on line 329 clashes
          // with encountering an error in the interpolated string.
          //
          // This clause should be the one taking care of the errors!
          ""
        }
        case _ if prev.head == INTERPOLATIONID =>
          accept("\"")
        case x => println(s"Unknown symbol: ${scanner.token}"); ???
      }

      while (scanner.token != EOF && scanner.token != STRINGLIT) {
        sb append scan
        prev = scanner.token :: prev
        scanner.nextToken()
      }

      val delim =
        if (scanner.inMultiLineInterpolation) "\"\"\""
        else "\""

      if (scanner.token == STRINGLIT) {
        // If the last part of an interpolated string is a literal, it will end
        // in `STRINGLIT`
        if (prev.head == INTERPOLATIONID) sb append literal(accept(delim))
        sb append literal(accept(scanner.strVal + delim))
      } else if (prev.head == ERROR && prev.tail.head != IDENTIFIER) {
        // If error entity to occur in an interpolation
        val litBuf = scanner.litBuf.toString
        val expectedLength = source.content.length
        realLength += litBuf.length
        val str =
          if (realLength + 4 == expectedLength) "\"\"\"" + litBuf + "$"
          else if (realLength + 3 == expectedLength) "\"\"\"" + litBuf
          else if (realLength + 2 == expectedLength) "\"" + litBuf + "$"
          else "\"" + litBuf

        sb append str
        prev = -1 :: prev.tail // make sure outer doesn't print this as well
      } else if (prev.head == ERROR && prev.tail.head == IDENTIFIER) {
        // If an error is preceeded by an identifier, i.e. later in the interpolation
        val litBuf = scanner.litBuf.toString
        val expLen = source.content.length

        val suffix = "" //TODO
        sb append (litBuf + suffix)
      } else if (prev.head == IDENTIFIER) {
        sb append scanner.litBuf.toString
      } else if (prev.head == INTERPOLATIONID && scanner.token == EOF) {
        sb append accept(delim)
        sb append accept(scanner.litBuf.toString)
      }
      sb.toString
    }

    buf.toVector
  }
}
