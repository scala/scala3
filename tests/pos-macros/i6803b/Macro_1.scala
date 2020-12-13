package blah

import scala.language.implicitConversions
import scala.quoted._

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given x: LineNo = ${impl}
    private def impl(using Quotes) : Expr[LineNo] = {
      import quotes.reflect._
      '{unsafe(${Expr(Position.ofMacroExpansion.startLine)})}
    }
  }
}
