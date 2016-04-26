package dotty.tools
package dotc
package repl

import parsing.Tokens._
import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList._
import ammonite.terminal.SpecialKeys._
import ammonite.terminal.Filter
import ammonite.terminal._
import scala.annotation.switch
import scala.collection.mutable.StringBuilder

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {
  val NoColor         = Console.RESET
  val CommentColor    = Console.GREEN
  val KeywordColor    = Console.CYAN
  val LiteralColor    = Console.MAGENTA
  val TypeColor       = Console.GREEN
  val AnnotationColor = Console.RED

  private def none(str: String)       = str
  private def keyword(str: String)    = KeywordColor + str + NoColor
  private def typeDef(str: String)    = TypeColor + str + NoColor
  private def literal(str: String)    = LiteralColor + str + NoColor
  private def annotation(str: String) = AnnotationColor + str + NoColor

  private val keywords: Seq[String] = for {
    index <- IF to FORSOME // All alpha keywords
  } yield tokenString(index)

  private val interpolationPrefixes =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' :: 'K' ::
    'L' :: 'M' :: 'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
    'W' :: 'X' :: 'Y' :: 'Z' :: '$' :: '_' :: 'a' :: 'b' :: 'c' :: 'd' :: 'e' ::
    'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 'p' ::
    'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: Nil

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

    while (iter.hasNext) {
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
          case '/'  =>
            if (iter.hasNext) {
              iter.next match {
                case '/' => eolComment()
                case '*' => blockComment()
                case x => {
                  newBuf += '/'
                  val (dup, _) = iter.duplicate
                  iter = List(x).toIterator ++ dup
                }
              }
            } else newBuf += '/'
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

    def eolComment() = {
      newBuf append (CommentColor + "//")
      var curr = '/'
      while (curr != '\n' && iter.hasNext) {
        curr = iter.next
        newBuf += curr
      }
      prev = curr
      newBuf append NoColor
    }

    def blockComment() = {
      newBuf append (CommentColor + "/*")
      var curr = '*'
      var open = 1
      while (open > 0 && iter.hasNext) {
        curr = iter.next
        newBuf += curr

        if (curr == '*' && iter.hasNext) {
          curr = iter.next
          newBuf += curr
          if (curr == '/') open -= 1
        } else if (curr == '/' && iter.hasNext) {
          curr = iter.next
          newBuf += curr
          if (curr == '*') open += 1
        }
      }
      prev = curr
      newBuf append NoColor
    }

    def appendLiteral(delim: Char, succ: Iterator[Char], multiline: Boolean = false) = {
      var curr: Char      = 0
      var continue        = true
      var closing         = 0
      val inInterpolation = interpolationPrefixes.contains(prev)
      newBuf append (LiteralColor + delim)

      def shouldInterpolate =
        inInterpolation && curr == '$' && prev != '$' && (iter.hasNext || succ.hasNext)

      def interpolate() = {
        val next: Char = if (succ.hasNext) succ.next else iter.next
        if (next == '$') {
          newBuf += curr
          newBuf += next
          prev = '$'
        } else if (next == '{') {
          newBuf append (KeywordColor + curr)
          newBuf += next
          if (iter.hasNext) {
            var c = iter.next
            while (iter.hasNext && c != '}') {
              newBuf += c
              c = iter.next
            }
            newBuf += c
            newBuf append LiteralColor
          }
        } else {
          newBuf append (KeywordColor + curr)
          newBuf += next
          var c: Char = 'a'
          while (c.isLetterOrDigit && (iter.hasNext || succ.hasNext)) {
            c = if (succ.hasNext) succ.next else iter.next
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

      while (continue && (iter.hasNext || succ.hasNext)) {
        curr = if(succ.hasNext) succ.next else iter.next
        if (curr == '\\' && (iter.hasNext || succ.hasNext)) {
          val next = if (succ.hasNext) succ.next else iter.next
          newBuf append (KeywordColor + curr)
          if (next == 'u') {
            val code = "u" + iter.take(4).mkString
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
      }
      newBuf append NoColor
      prev = curr

      if (succ.hasNext) {
        val (dup, _) = iter.duplicate
        iter = succ ++ dup
      }
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
}
