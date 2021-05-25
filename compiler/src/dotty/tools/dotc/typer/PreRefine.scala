package dotty.tools.dotc
package typer

import core.Phases.Phase
import core.DenotTransformers.IdentityDenotTransformer
import core.Contexts.Context

/** A phase that precedes the refiner and that allows installing
 *  completers for local symbols
 */
class PreRefine extends Phase, IdentityDenotTransformer:

  def phaseName: String = "preRefine"

  def run(using Context): Unit =
    assert(next.isInstanceOf[RefineTypes],
      s"misconfigured phases: phase PreRefine must be followed by phase RefineTypes")

  override def isCheckable = false
