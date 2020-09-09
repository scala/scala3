package dotty.tools.dotc.config

import dotty.tools.dotc.core._
import Contexts._
import Symbols._
import SymDenotations._

import dotty.tools.backend.sjs.JSDefinitions

import org.scalajs.ir.Trees.JSNativeLoadSpec

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
      || jsDefinitions.isJSFunctionClass(cls)
      || jsDefinitions.isJSThisFunctionClass(cls)

  /** Is the given class assured by the platform not to have any initialization code? */
  override def isAssuredNoInits(sym: ClassSymbol)(using Context): Boolean =
    super.isAssuredNoInits(sym) || sym.hasAnnotation(jsDefinitions.JSGlobalScopeAnnot)

  override def shouldReceiveJavaSerializationMethods(sym: ClassSymbol)(using Context): Boolean =
    !sym.isSubClass(jsDefinitions.JSAnyClass)

  object perRunInfo {
    private val jsNativeLoadSpecs = new MutableSymbolMap[JSNativeLoadSpec]

    /** Clears all the info at the beginning of a run. */
    def clear(): Unit =
      jsNativeLoadSpecs.clear()

    /** Stores the JS native load spec of a symbol for the current compilation run. */
    def storeJSNativeLoadSpec(sym: Symbol, spec: JSNativeLoadSpec): Unit =
      jsNativeLoadSpecs(sym) = spec

    /** Gets the JS native load spec of a symbol in the current compilation run. */
    def jsNativeLoadSpecOf(sym: Symbol): JSNativeLoadSpec =
      jsNativeLoadSpecs(sym)

    /** Gets the JS native load spec of a symbol in the current compilation run, if it has one. */
    def jsNativeLoadSpecOfOption(sym: Symbol): Option[JSNativeLoadSpec] =
      jsNativeLoadSpecs.get(sym)
  }
}
