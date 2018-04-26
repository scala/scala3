package scala.tasty

import scala.runtime.tasty.Toolbox

trait Context {
  protected[tasty] def toolbox: Toolbox
}
