package blah

import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.autolift

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given x as LineNo = ${impl}
    private def impl(using qctx: QuoteContext) : Expr[LineNo] = {
      import qctx.tasty._
      '{unsafe(${rootPosition.startLine})}
    }
  }
}
