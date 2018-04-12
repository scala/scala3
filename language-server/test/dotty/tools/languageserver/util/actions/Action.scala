package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.server.TestServer

import PositionContext._

/**
 * Base trait for representing an action performed against a language server (such as hover, go to
 * definition, etc.)
 */
trait Action {
  type Exec[T] = implicit (TestServer, PositionContext) => T

  /** Execute the action. */
  def execute(): Exec[Unit]

  /** Return a textual representation of this action. */
  def show: PosCtx[String]

  /** The server that this action targets. */
  def server: Exec[DottyLanguageServer] = implicitly[TestServer].server

}
