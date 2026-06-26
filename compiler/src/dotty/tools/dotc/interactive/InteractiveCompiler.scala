package dotty.tools
package dotc
package interactive

import core.*
import Phases.*
import parsing.*
import typer.*

class InteractiveCompiler extends Compiler {
  // TODO: Figure out what phases should be run in IDEs
  // More phases increase latency but allow us to report more errors.
  // This could be improved by reporting errors back to the IDE
  // after each phase group instead of waiting for the pipeline to finish.
  override def phases: Vector[Vector[Phase]] = Vector(
    Vector(new Parser),
    Vector(new TyperPhase),
    Vector(new transform.SetRootTree),
    Vector(new transform.CookComments)
  )
}
