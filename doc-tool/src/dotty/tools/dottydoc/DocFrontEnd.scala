package dotty.tools
package dottydoc

import dotc.typer.FrontEnd
import dotc.core.Contexts.Context
import dotc.CompilationUnit

/** `DocFrontEnd` uses the Dotty `FrontEnd` without discarding the AnyVal
  * interfaces for Boolean, Int, Char, Long, Byte etc.
  *
  * It currently still throws away Java sources by overriding
  * `discardAfterTyper`.
  */
class DocFrontEnd extends FrontEnd {
  override protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava
}
