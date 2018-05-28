package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker

/** An action that defines a `marker` where the action will be performed. */
trait ActionOnMarker extends Action {

  /** The marker that defines where the action will be performed. */
  def marker: CodeMarker

}
