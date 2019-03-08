package dotty.semanticdb

import scala.io.Source
import scala.math._
import dotty.tools.dotc.util.SourceFile

class SourceFile(path: java.nio.file.Path) {
  private val sourceCode = Source.fromFile(path.toFile).mkString
  private val sourceFile = SourceFile.virtual(path.toString, sourceCode)

  def content() = sourceCode

  def offsetToLine(offset: Int): Int = sourceFile.offsetToLine(offset)

  def lineToOffset(offset: Int): Int = sourceFile.lineToOffset(offset)

  def peek(start: Int, end: Int) : String =
    sourceCode.substring(max(start, 0), min(end, sourceCode.length - 1))

  def firstOccurrenceLetter(letter: Char, start : Int) : Int = {
    var pos = start
    while (pos < sourceCode.length && sourceCode(pos) != letter) {
      val nextPos = nextCharacterSkipComments(pos)
      if (nextPos == pos)
        pos = nextPos + 1
      else
        pos = nextPos
    }
    return pos
  }

  def nextCharacterSkipComments(start : Int): Int = {
    def aux(start : Int) : Int = {
        var i = start
        if (i+2 <= sourceCode.length && sourceCode.substring(i, i+2) == "//") {
          while (i < sourceCode.length && sourceCode(i) != '\n')
            i += 1
          return i+1
        } else if (i+2 <= sourceCode.length && sourceCode.substring(i, i+2) == "/*") {
          var nestedCount = 0
          i += 2
          while (i + 2 <= sourceCode.length &&
                 !(sourceCode.substring(i, i+2) == "*/" &&
                 nestedCount == 0)) {
            val s = sourceCode.substring(i, i+2)
            if (s == "/*") {
              nestedCount += 1
              i += 1
            }
            if (s == "*/") {
              nestedCount -= 1
              i += 1
            }
            i += 1
          }
          return i+2
        } else {
          while (i < sourceCode.length && sourceCode(i).isWhitespace)
            i += 1
          return i
        }
      }
      var previous = start
      var next = aux(previous)
      while (previous != next) {
        previous = next
        next = aux(previous)
      }
      return previous
    }
}