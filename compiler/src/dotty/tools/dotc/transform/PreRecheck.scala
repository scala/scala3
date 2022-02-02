package dotty.tools.dotc
package transform

import core.Phases.Phase
import core.DenotTransformers.DenotTransformer
import core.Contexts.{Context, ctx}

/** A phase that precedes the rechecker and that allows installing
 *  new types for local symbols.
 */
abstract class PreRecheck extends Phase, DenotTransformer:

  def phaseName: String = "preRecheck"

  override def changesBaseTypes: Boolean = true

  def run(using Context): Unit = ()

  override def isCheckable = false
