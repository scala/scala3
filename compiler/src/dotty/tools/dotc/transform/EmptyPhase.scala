package dotty.tools.dotc
package transform

import core.*
import Contexts.Context
import Phases.Phase

/** A phase that can be inserted directly after a phase that cannot
 *  be checked, to enable a -Ycheck as soon as possible afterwards
 */
class EmptyPhase extends Phase:

  def phaseName: String = "dummy"

  override def isEnabled(using Context) = prev.isEnabled

  override def run(using Context) = ()

end EmptyPhase