package dotty.tools.dotc
package transform

import core.Phases.Phase
import core.DenotTransformers.IdentityDenotTransformer
import core.Contexts.{Context, ctx}

/** A phase that precedes the rechecker and that allows installing
 *  new types for local symbols.
 */
class PreRecheck extends Phase, IdentityDenotTransformer:

  def phaseName: String = "preRecheck"

  override def isEnabled(using Context) = next.isEnabled

  override def changesBaseTypes: Boolean = true

  def run(using Context): Unit = ()

  override def isCheckable = false
