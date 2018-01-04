package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.server.TestServer

import PositionContext._

trait Action {
  type Exec[T] = implicit (TestServer, PositionContext) => T
  def execute(): Exec[Unit]
  def show: PosCtx[String]
  def server: Exec[DottyLanguageServer] = implicitly[TestServer].server

  // FIXME
  // Workaroud an issue with implicit functions and phantomArgLift.
  // That phase will dispear which should fix this issue. Just remove calls to param when that hapens
  protected def fix[T](f: PositionContext => T): PosCtx[T] = f(implicitly[PositionContext])
}
