package dotty.tools
package dotc

import core.Contexts.Context

/* To do:
 */
object Main extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new Compiler
}
