package dotty.tools.dotc
package reporting
package typer

import core.Contexts._
import core.Types._
import util.SourcePosition
import reporting.Severity._
import dotty.tools.dotc.typer.ErrorReporting.InfoString

/** all typer error classes */
object errors {

  class NotStableError private[errors] (msgFn: => String, pos: SourcePosition, base: ContextBase)
    extends TyperError(msgFn, pos, base)
  
  def NotStableError(tp: Type, pos: SourcePosition)(implicit ctx: Context) =
    new NotStableError(i"$tp is not stable", pos, ctx.base)

}
