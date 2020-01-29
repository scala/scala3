package blah

import scala.language.implicitConversions
import scala.quoted._

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given LineNo = ${impl}
    private def impl with (qctx: QuoteContext): Expr[LineNo] = {
      import qctx.tasty.{given, _}
      '{unsafe(${Expr(rootPosition.startLine)})}
    }
  }
}

package AsPackage {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given LineNo = ${impl}
    private def impl with (qctx: QuoteContext): Expr[LineNo] = {
      import qctx.tasty.{given, _}
      '{unsafe(${Expr(rootPosition.startLine)})}
    }
  }
}
