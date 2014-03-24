package dotty.tools.dotc
package reporting
package parser

import core.Contexts._
import util.SourcePosition
import reporting.Severity._

/** all parser error classes */
object errors {

  class ImplicitToplevelClass private[errors] (pos: SourcePosition, base: ContextBase) extends
    ParserError("implicit classes may not be toplevel", pos, base)

  def ImplicitToplevelClass(pos: SourcePosition)(implicit ctx: Context) =
    new ImplicitToplevelClass(pos, ctx.base)

  class ImplicitCaseClass private[errors] (pos: SourcePosition, base: ContextBase) extends
    ParserError("implicit classes may not be case classes", pos, base)

  def ImplicitCaseClass(pos: SourcePosition)(implicit ctx: Context) =
    new ImplicitCaseClass(pos, ctx.base)

}
