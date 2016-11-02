package dotty.tools
package dotc

import core.Contexts.Context

/** Main class of the `dotc` batch compiler. */
object Main extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new Compiler
}
