package dotty.tools
package dotc
package printing

import parsing.Tokens._
import scala.annotation.switch
import scala.collection.mutable.StringBuilder
import core.Contexts.Context
import util.Chars
import Highlighting.{Highlight, HighlightBuffer}
import dotty.tools.dotc.printing.Texts._

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {

  val NoColor         = Console.RESET
  val CommentColor    = Console.BLUE
  val KeywordColor    = Console.YELLOW
  val ValDefColor     = Console.CYAN
  val LiteralColor    = Console.RED
  val StringColor     = Console.GREEN
  val TypeColor       = Console.MAGENTA
  val AnnotationColor = Console.MAGENTA

  private def none(str: String) = str
  private def keyword(str: String) = KeywordColor + str + NoColor
  private def typeDef(str: String) = TypeColor + str + NoColor
  private def literal(str: String) = LiteralColor + str + NoColor
  private def valDef(str: String) = ValDefColor + str + NoColor
  private def operator(str: String) = TypeColor + str + NoColor
  private def annotation(str: String) =
    if (str.trim == "@") str else { AnnotationColor + str + NoColor }
  private val tripleQs = Console.RED_B + "???" + NoColor

  private val keywords: Seq[String] = for {
    index <- IF to ENUM // All alpha keywords
  } yield tokenString(index)

  private val interpolationPrefixes =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' :: 'K' ::
    'L' :: 'M' :: 'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
    'W' :: 'X' :: 'Y' :: 'Z' :: '$' :: '_' :: 'a' :: 'b' :: 'c' :: 'd' :: 'e' ::
    'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 'p' ::
    'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: Nil

  private val typeEnders =
   '{'  :: '}' :: ')' :: '(' :: '[' :: ']' :: '=' :: ' ' :: ',' :: '.' ::
   '\n' :: Nil

  def apply(chars: Iterable[Char]): Iterable[Char] = {
    var prev: Char = 0
    var remaining  = chars.toStream
    val newBuf     = new StringBuilder
    var lastToken  = ""

    @inline def keywordStart =
      prev == 0    || prev == ' ' || prev == '{' || prev == '(' ||
      prev == '\n' || prev == '[' || prev == ','

    @inline def numberStart(c: Char) =
      c.isDigit && (!prev.isLetter || prev == '.' || prev == ' ' || prev == '(' || prev == '\u0000')

    def takeChar(): Char = takeChars(1).head
    def takeChars(x: Int): Seq[Char] = {
      val taken = remaining.take(x)
      remaining = remaining.drop(x)
      taken
    }

    while (remaining.nonEmpty) {
      val n = takeChar()
      if (interpolationPrefixes.contains(n)) {
        // Interpolation prefixes are a superset of the keyword start chars
        val next = remaining.take(3).mkString
        if (next.startsWith("\"")) {
          newBuf += n
          prev = n
          if (remaining.nonEmpty) takeChar() // drop 1 for appendLiteral
          appendString('"', next == "\"\"\"")
        } else {
          if (n.isUpper && (keywordStart || prev == '.')) {
            appendWhile(n, !typeEnders.contains(_), typeDef)
          } else if (keywordStart) {
            append(n, keywords.contains(_), { kw =>
              if (kw == "new") typeDef(kw) else keyword(kw)
            })
          } else {
            newBuf += n
            prev = n
          }
        }
      } else {
        (n: @switch) match {
          case '/'  =>
            if (remaining.nonEmpty) {
              remaining.head match {
                case '/' =>
                  takeChar()
                  eolComment()
                case '*' =>
                  takeChar()
                  blockComment()
                case x =>
                  newBuf += '/'
              }
            } else newBuf += '/'
          case '='  =>
            append('=', _ == "=>", operator)
          case '<'  =>
            append('<', { x => x == "<-" || x == "<:" || x == "<%" }, operator)
          case '>'  =>
            append('>', { x => x == ">:" }, operator)
          case '#'  =>
            if (prev != ' ' && prev != '.') newBuf append operator("#")
            else newBuf += n
            prev = '#'
          case '@'  =>
            appendWhile('@', !typeEnders.contains(_), annotation)
          case '\"' =>
            appendString('\"', multiline = remaining.take(2).mkString == "\"\"")
          case '\'' =>
            appendSingleQuote('\'')
          case '`'  =>
            appendTo('`', _ == '`', none)
          case _    => {
            if (n == '?' && remaining.take(2).mkString == "??") {
              takeChars(2)
              newBuf append tripleQs
              prev = '?'
            }
            else if (n.isUpper && keywordStart)
              appendWhile(n, !typeEnders.contains(_), typeDef)
            else if (numberStart(n))
              appendWhile(n, { x => x.isDigit || x == '.' || x == '\u0000'}, literal)
            else
              newBuf += n; prev = n
          }
        }
      }
    }

    def eolComment() = {
      newBuf append (CommentColor + "//")
      var curr = '/'
      while (curr != '\n' && remaining.nonEmpty) {
        curr = takeChar()
        newBuf += curr
      }
      prev = curr
      newBuf append NoColor
    }

    def blockComment() = {
      newBuf append (CommentColor + "/*")
      var curr = '*'
      var open = 1
      while (open > 0 && remaining.nonEmpty) {
        curr = takeChar()
        if (curr == '@') {
          appendWhile('@', !typeEnders.contains(_), annotation)
          newBuf append CommentColor
        }
        else newBuf += curr

        if (curr == '*' && remaining.nonEmpty) {
          curr = takeChar()
          newBuf += curr
          if (curr == '/') open -= 1
        } else if (curr == '/' && remaining.nonEmpty) {
          curr = takeChar()
          newBuf += curr
          if (curr == '*') open += 1
        }

        if (Chars.isLineBreakChar(curr)) {
          newBuf append CommentColor
        }
      }
      prev = curr
      newBuf append NoColor
    }

    def appendString(delim: Char, multiline: Boolean = false) = {
      var curr: Char      = 0
      var continue        = true
      var closing         = 0
      val inInterpolation = interpolationPrefixes.contains(prev)
      newBuf append (LiteralColor + delim)

      def shouldInterpolate =
        inInterpolation && curr == '$' && prev != '$' && remaining.nonEmpty

      def interpolate() = {
        val next = takeChar()
        if (next == '$') {
          newBuf += curr
          newBuf += next
          prev = '$'
        } else if (next == '{') {
          var open = 1 // keep track of open blocks
          newBuf append (ValDefColor + curr)
          newBuf += next
          while (remaining.nonEmpty && open > 0) {
            var c = takeChar()
            newBuf += c
            if (c == '}') open -= 1
            else if (c == '{') open += 1
          }
          newBuf append LiteralColor
        } else {
          newBuf append (ValDefColor + curr)
          newBuf += next
          var c: Char = 'a'
          while (c.isLetterOrDigit && remaining.nonEmpty) {
            c = takeChar()
            if (c != '"') newBuf += c
          }
          newBuf append LiteralColor
          if (c == '"') {
            newBuf += c
            continue = false
          }
        }
        closing = 0
      }

      while (continue && remaining.nonEmpty) {
        curr = takeChar()
        if (curr == '\\' && remaining.nonEmpty) {
          val next = takeChar()
          newBuf append (KeywordColor + curr)
          if (next == 'u') {
            val code = "u" + takeChars(4).mkString
            newBuf append code
          } else newBuf += next
          newBuf append LiteralColor
          closing = 0
        } else if (shouldInterpolate) {
          interpolate()
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

        if (Chars.isLineBreakChar(curr)) {
          newBuf append LiteralColor
        }
      }
      newBuf append NoColor
      prev = curr
    }

    def appendSingleQuote(delim: Char) = remaining.take(3) match {
      case chr #:: '\'' #:: _ => // single character
        newBuf append LiteralColor
        newBuf appendAll s"'$chr'"
        newBuf append NoColor
        takeChars(2)
        prev = '\''
      case '\\' #:: chr #:: '\'' #:: _ => // escaped character
        newBuf append LiteralColor
        newBuf appendAll s"'\\$chr'"
        newBuf append NoColor
        takeChars(3)
        prev = '\''
      case _ => appendWhile(delim, !typeEnders.contains(_), literal)
    }

    def append(c: Char, shouldHL: String => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")

      def delim(c: Char) = (c: @switch) match {
        case ' ' => true
        case '\n' => true
        case '(' => true
        case '[' => true
        case ':' => true
        case '@' => true
        case '.' => true
        case _ => false
      }

      while (remaining.nonEmpty && !delim(curr)) {
        curr = takeChar()
        if (!delim(curr)) sb += curr
      }

      val str    = sb.toString
      val toAdd  =
        if (shouldHL(str))
          highlight(str)
        else if (("var" :: "val" :: "def" :: "case" :: Nil).contains(lastToken))
          valDef(str)
        else str
      val suffix = if (delim(curr)) s"$curr" else ""
      newBuf append (toAdd + suffix)
      lastToken = str
      prev = curr
    }

    def appendWhile(c: Char, pred: Char => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while (remaining.nonEmpty && pred(curr)) {
        curr = takeChar()
        if (pred(curr)) sb += curr
      }

      val str    = sb.toString
      val suffix = if (!pred(curr)) s"$curr" else ""
      newBuf append (highlight(str) + suffix)
      prev = curr
    }

    def appendTo(c: Char, pred: Char => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while (remaining.nonEmpty && !pred(curr)) {
        curr = takeChar()
        sb += curr
      }

      newBuf append highlight(sb.toString)
      prev = curr
    }

    newBuf.toIterable
  }
}
