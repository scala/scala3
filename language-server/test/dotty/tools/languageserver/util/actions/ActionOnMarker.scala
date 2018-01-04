package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker

trait ActionOnMarker extends Action {

  def marker: CodeMarker

}
