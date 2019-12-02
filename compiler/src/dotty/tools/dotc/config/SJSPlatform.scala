package dotty.tools.dotc.config

import dotty.tools.dotc.core._
import Contexts._
import Symbols._

import dotty.tools.backend.sjs.JSDefinitions

class SJSPlatform()(implicit ctx: Context) extends JavaPlatform {

  /** Scala.js-specific definitions. */
  val jsDefinitions: JSDefinitions = new JSDefinitions()

  /** Is the SAMType `cls` also a SAM under the rules of the Scala.js back-end? */
  override def isSam(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    defn.isFunctionClass(cls) || jsDefinitions.isJSFunctionClass(cls)
}

