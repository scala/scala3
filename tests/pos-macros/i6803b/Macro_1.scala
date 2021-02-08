package blah

import scala.language.implicitConversions
import scala.quoted.*

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given x: LineNo = ${impl}
    private def impl(using Quotes) : Expr[LineNo] = {
      import quotes.reflect.*
      '{unsafe(${Expr(Position.ofMacroExpansion.startLine)})}
    }
  }
}
