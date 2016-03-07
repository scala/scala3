package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Flags._
import Symbols._
import NameOps._
import StdNames._

import JSDefinitions._

/** Management of the interoperability with JavaScript. */
object JSInterop {

  /** Is this symbol a JavaScript type? */
  def isJSType(sym: Symbol)(implicit ctx: Context): Boolean = {
    //sym.hasAnnotation(jsdefn.RawJSTypeAnnot)
    ctx.atPhase(ctx.erasurePhase) { implicit ctx =>
      sym.derivesFrom(jsdefn.JSAnyClass)
    }
  }

  /** Is this symbol a Scala.js-defined JS class, i.e., a non-native JS class? */
  def isScalaJSDefinedJSClass(sym: Symbol)(implicit ctx: Context): Boolean =
    isJSType(sym) && !sym.hasAnnotation(jsdefn.JSNativeAnnot)

}
