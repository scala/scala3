package dottyBench.tools.dotc.config

import dottyBench.tools.dotc.core._
import Contexts._
import Symbols._

import dottyBench.tools.backend.sjs.JSDefinitions

class SJSPlatform()(using Ctx, CState) extends JavaPlatform {

  /** Scala.js-specific definitions. */
  val jsDefinitions: JSDefinitions = new JSDefinitions()

  /** Is the SAMType `cls` also a SAM under the rules of the Scala.js back-end? */
  override def isSam(cls: ClassSymbol)(using Ctx, CState): Boolean =
    defn.isFunctionClass(cls) || jsDefinitions.isJSFunctionClass(cls)
}

