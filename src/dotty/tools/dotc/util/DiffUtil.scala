package dotty.tools.dotc.util

import scala.annotation.tailrec
import difflib._

object DiffUtil {

  private final val ANSI_DEFAULT = "\u001B[0m"
  private final val ANSI_RED = "\u001B[31m"
  private final val ANSI_GREEN = "\u001B[32m"

  private final val DELETION_COLOR = ANSI_RED
  private final val ADDITION_COLOR = ANSI_GREEN

  def mkColoredCodeDiff(code: String, lastCode: String, printDiffDel: Boolean): String = {
    import scala.collection.JavaConversions._

    @tailrec def split(str: String, acc: List[String]): List[String] = {
      if (str == "") {
        acc.reverse
      } else {
        val head = str.charAt(0)
        val (token, rest) = if (Character.isAlphabetic(head) || Character.isDigit(head)) {
          str.span(c => Character.isAlphabetic(c) || Character.isDigit(c))
        } else if (Character.isMirrored(head) || Character.isWhitespace(head)) {
          str.splitAt(1)
        } else {
          str.span { c =>
            !Character.isAlphabetic(c) && !Character.isDigit(c) &&
              !Character.isMirrored(c) && !Character.isWhitespace(c)
          }
        }
        split(rest, token :: acc)
      }
    }

    val lines = split(code, Nil).toArray
    val diff = DiffUtils.diff(split(lastCode, Nil), lines.toList)

    for (delta <- diff.getDeltas) {
      val pos = delta.getRevised.getPosition
      val endPos = pos + delta.getRevised.getLines.size - 1

      delta.getType.toString match { // Issue #1355 forces us to use the toString
        case "INSERT" =>
          lines(pos) = ADDITION_COLOR + lines(pos)
          lines(endPos) = lines(endPos) + ANSI_DEFAULT

        case "CHANGE" =>
          val old = if (!printDiffDel) "" else
            DELETION_COLOR + delta.getOriginal.getLines.mkString + ANSI_DEFAULT
          lines(pos) = old + ADDITION_COLOR + lines(pos)
          lines(endPos) = lines(endPos) + ANSI_DEFAULT

        case "DELETE" if printDiffDel =>
          val deleted = delta.getOriginal.getLines.mkString
          if (!deleted.forall(Character.isWhitespace)) {
            lines(pos) = DELETION_COLOR + deleted + ANSI_DEFAULT + lines(pos)
          }

        case _ =>
      }
    }

    lines.mkString
  }
}
