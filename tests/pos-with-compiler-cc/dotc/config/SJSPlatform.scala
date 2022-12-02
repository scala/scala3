package dotty.tools.dotc.config

import dotty.tools.dotc.core._
import Contexts._
import Symbols._

import dotty.tools.backend.sjs.JSDefinitions

object SJSPlatform {
  /** The `SJSPlatform` for the current context. */
  def sjsPlatform(using Context): SJSPlatform =
    ctx.platform.asInstanceOf[SJSPlatform]
}

class SJSPlatform()(using Context) extends JavaPlatform {

  /** Scala.js-specific definitions. */
  val jsDefinitions: JSDefinitions = new JSDefinitions()

  /** Is the SAMType `cls` also a SAM under the rules of the Scala.js back-end? */
  override def isSam(cls: ClassSymbol)(using Context): Boolean =
    defn.isFunctionClass(cls)
      || cls.superClass == jsDefinitions.JSFunctionClass

  /** Is the given class symbol eligible for Java serialization-specific methods?
   *
   *  This is not simply false because we still want to add them to Scala classes
   *  and objects. They might be transitively used by macros and other compile-time
   *  code. It feels safer to have them be somewhat equivalent to the ones we would
   *  get in a JVM project. The JVM back-end will slap an extends `java.io.Serializable`
   *  to them, so we should be consistent and also emit the proper serialization methods.
   */
  override def shouldReceiveJavaSerializationMethods(sym: ClassSymbol)(using Context): Boolean =
    !sym.isSubClass(jsDefinitions.JSAnyClass)
}
