package blah

import scala.language.implicitConversions
import scala.quoted._
import given scala.quoted.autolift._

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given x as LineNo = ${impl}
    private def impl given (qctx: QuoteContext): Expr[LineNo] = {
      import qctx.tasty._
      '{unsafe(${rootPosition.startLine})}
    }
  }
}
