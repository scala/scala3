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

  /** Should this symbol be translated into a JS getter? */
  def isJSGetter(sym: Symbol)(implicit ctx: Context): Boolean = {
    sym.info.firstParamTypes.isEmpty && ctx.atPhase(ctx.erasurePhase) { implicit ctx =>
      sym.info.isParameterless
    }
  }

  /** Should this symbol be translated into a JS setter? */
  def isJSSetter(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.name.isSetterName && sym.is(Method)

  /** Should this symbol be translated into a JS bracket access? */
  def isJSBracketAccess(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.hasAnnotation(jsdefn.JSBracketAccessAnnot)

  /** Should this symbol be translated into a JS bracket call? */
  def isJSBracketCall(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.hasAnnotation(jsdefn.JSBracketCallAnnot)

  /** Is this symbol a default param accessor for a JS method?
   *
   *  For default param accessors of *constructors*, we need to test whether
   *  the companion *class* of the owner is a JS type; not whether the owner
   *  is a JS type.
   */
  def isJSDefaultParam(sym: Symbol)(implicit ctx: Context): Boolean = {
    sym.name.isDefaultGetterName && {
      val owner = sym.owner
      if (owner.is(ModuleClass) &&
          sym.name.asTermName.defaultGetterToMethod == nme.CONSTRUCTOR) {
        isJSType(owner.linkedClass)
      } else {
        isJSType(owner)
      }
    }
  }

  /** Gets the unqualified JS name of a symbol.
   *
   *  If it is not explicitly specified with an `@JSName` annotation, the
   *  JS name is inferred from the Scala name.
   */
  def jsNameOf(sym: Symbol)(implicit ctx: Context): String = {
    sym.getAnnotation(jsdefn.JSNameAnnot).flatMap(_.argumentConstant(0)).fold {
      val base = sym.name.unexpandedName.decode.toString.stripSuffix("_=")
      if (sym.is(ModuleClass)) base.stripSuffix("$")
      else if (!sym.is(Method)) base.stripSuffix(" ")
      else base
    } { constant =>
      constant.stringValue
    }
  }

  /** Gets the fully qualified JS name of a static class of module Symbol.
   *
   *  This is the JS name of the symbol qualified by the fully qualified JS
   *  name of its original owner if the latter is a native JS object.
   */
  def fullJSNameOf(sym: Symbol)(implicit ctx: Context): String = {
    assert(sym.isClass, s"fullJSNameOf called for non-class symbol $sym")
    sym.getAnnotation(jsdefn.JSFullNameAnnot).flatMap(_.argumentConstant(0)).fold {
      jsNameOf(sym)
    } { constant =>
      constant.stringValue
    }
  }

}
