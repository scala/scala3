package dotty.tools
package dotc

import core.Contexts.Context
import reporting.Reporter

/* To do:
 */
object Main extends Driver {
  override def newCompiler(): Compiler = new Compiler
}
