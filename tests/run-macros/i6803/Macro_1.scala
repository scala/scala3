package blah

import scala.language.implicitConversions
import scala.quoted._

object AsObject {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given LineNo = ${impl}
    private def impl(using Quotes): Expr[LineNo] = {
      import qctx.reflect._
      '{unsafe(${Expr(Position.ofMacroExpansion.startLine)})}
    }
  }
}

package AsPackage {
  final class LineNo(val lineNo: Int)
  object LineNo {
    def unsafe(i: Int): LineNo = new LineNo(i)
    inline given LineNo = ${impl}
    private def impl(using Quotes): Expr[LineNo] = {
      import qctx.reflect._
      '{unsafe(${Expr(Position.ofMacroExpansion.startLine)})}
    }
  }
}
