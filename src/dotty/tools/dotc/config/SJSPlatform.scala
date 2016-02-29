package dotty.tools.dotc.config

import dotty.tools.dotc.core._
import Contexts._

import dotty.tools.backend.sjs.JSDefinitions

class SJSPlatform()(implicit ctx: Context) extends JavaPlatform {

  /** Scala.js-specific definitions. */
  val jsDefinitions: JSDefinitions = new JSDefinitions()

}
