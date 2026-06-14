package dotty.tools
package backend.jvm

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.util.SrcPos

/**
 * Abstracts the frontend data structures, specially the Context, that need to be accessed in a single-threaded manner.
 */
final class PostProcessorFrontendAccess(val ctx: Context) {
  def optimizerWarning(msg: Context ?=> Message, site: String, pos: SrcPos): Unit =
    report.optimizerWarning(msg(using ctx), site, pos)(using ctx)
}