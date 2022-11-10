package dotty.tools
package dotc
package interactive

import core._
import Phases._
import parsing._
import typer._

class InteractiveCompiler extends Compiler {
  // TODO: Figure out what phases should be run in IDEs
  // More phases increase latency but allow us to report more errors.
  // This could be improved by reporting errors back to the IDE
  // after each phase group instead of waiting for the pipeline to finish.
  override def phases: List[List[Phase]] = List(
    List(new Parser),
    List(new TyperPhase),
    List(new transform.SetRootTree),
    List(new transform.CookComments)
  )
}
