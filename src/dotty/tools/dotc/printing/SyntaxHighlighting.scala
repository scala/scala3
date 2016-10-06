package dotty.tools
package dotc
package printing

import parsing.Tokens._
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
    index <- IF to INLINE // All alpha keywords
  } yield tokenString(index)

  private val interpolationPrefixes =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' :: 'K' ::
    'L' :: 'M' :: 'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
    'W' :: 'X' :: 'Y' :: 'Z' :: '$' :: '_' :: 'a' :: 'b' :: 'c' :: 'd' :: 'e' ::
    'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 'p' ::
    'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: Nil

  private val typeEnders =
   '{' :: '}' :: ')' :: '(' :: '=' :: ' ' :: ',' :: '.' :: '\n' :: Nil

  def apply(chars: Iterable[Char]): Vector[Char] = {
    var prev: Char = 0
    var remaining  = chars.toStream
    val newBuf     = new StringBuilder

    @inline def keywordStart =
      prev == 0 || prev == ' ' || prev == '{' || prev == '(' || prev == '\n'

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
          appendLiteral('"', next == "\"\"\"")
        } else {
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
            append('=', _ == "=>", keyword)
          case '<'  =>
            append('<', { x => x == "<-" || x == "<:" || x == "<%" }, keyword)
          case '>'  =>
            append('>', { x => x == ">:" }, keyword)
          case '#'  =>
            if (prev != ' ' && prev != '.') newBuf append keyword("#")
            else newBuf += n
            prev = '#'
          case '@'  =>
            appendWhile('@', _ != ' ', annotation)
          case '\"' =>
            appendLiteral('\"', multiline = remaining.take(2).mkString == "\"\"")
          case '\'' =>
            appendLiteral('\'')
          case '`'  =>
            appendTo('`', _ == '`', none)
          case _    => {
            if (n.isUpper && keywordStart)
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
        newBuf += curr

        if (curr == '*' && remaining.nonEmpty) {
          curr = takeChar()
          newBuf += curr
          if (curr == '/') open -= 1
        } else if (curr == '/' && remaining.nonEmpty) {
          curr = takeChar()
          newBuf += curr
          if (curr == '*') open += 1
        }
      }
      prev = curr
      newBuf append NoColor
    }

    def appendLiteral(delim: Char, multiline: Boolean = false) = {
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
          newBuf append (KeywordColor + curr)
          newBuf += next
          while (remaining.nonEmpty && open > 0) {
            var c = takeChar()
            newBuf += c
            if (c == '}') open -= 1
            else if (c == '{') open += 1
          }
          newBuf append LiteralColor
        } else {
          newBuf append (KeywordColor + curr)
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
      }
      newBuf append NoColor
      prev = curr
    }

    def append(c: Char, shouldHL: String => Boolean, highlight: String => String) = {
      var curr: Char = 0
      val sb = new StringBuilder(s"$c")
      while (remaining.nonEmpty && curr != ' ' && curr != '(' && curr != '\n') {
        curr = takeChar()
        if (curr != ' ' && curr != '\n') sb += curr
      }

      val str    = sb.toString
      val toAdd  = if (shouldHL(str)) highlight(str) else str
      val suffix = if (curr == ' ' || curr == '\n') s"$curr" else ""
      newBuf append (toAdd + suffix)
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

    newBuf.toVector
  }
}
