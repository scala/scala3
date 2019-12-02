package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Flags._
import Symbols._
import NameOps._
import StdNames._
import NameKinds.DefaultGetterName

import JSDefinitions._

/** Management of the interoperability with JavaScript. */
object JSInterop {

  /** Is this symbol a JavaScript type? */
  def isJSType(sym: Symbol)(implicit ctx: Context): Boolean = {
    //sym.hasAnnotation(jsdefn.RawJSTypeAnnot)
    ctx.atPhase(ctx.erasurePhase) {
      sym.derivesFrom(jsdefn.JSAnyClass)
    }
  }

  /** Is this symbol a Scala.js-defined JS class, i.e., a non-native JS class? */
  def isScalaJSDefinedJSClass(sym: Symbol)(implicit ctx: Context): Boolean =
    isJSType(sym) && !sym.hasAnnotation(jsdefn.JSNativeAnnot)

  /** Should this symbol be translated into a JS getter?
   *
   *  This is true for any parameterless method, i.e., defined without `()`.
   *  Unlike `SymDenotations.isGetter`, it applies to user-defined methods as
   *  much as *accessor* methods created for `val`s and `var`s.
   */
  def isJSGetter(sym: Symbol)(implicit ctx: Context): Boolean = {
    sym.info.firstParamTypes.isEmpty && ctx.atPhase(ctx.erasurePhase) {
      sym.info.isParameterless
    }
  }

  /** Should this symbol be translated into a JS setter?
   *
   *  This is true for any method whose name ends in `_=`.
   *  Unlike `SymDenotations.isGetter`, it applies to user-defined methods as
   *  much as *accessor* methods created for `var`s.
   */
  def isJSSetter(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.name.isSetterName && sym.is(Method)

  /** Should this symbol be translated into a JS bracket access?
   *
   *  This is true for methods annotated with `@JSBracketAccess`.
   */
  def isJSBracketAccess(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.hasAnnotation(jsdefn.JSBracketAccessAnnot)

  /** Should this symbol be translated into a JS bracket call?
   *
   *  This is true for methods annotated with `@JSBracketCall`.
   */
  def isJSBracketCall(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.hasAnnotation(jsdefn.JSBracketCallAnnot)

  /** Is this symbol a default param accessor for a JS method?
   *
   *  For default param accessors of *constructors*, we need to test whether
   *  the companion *class* of the owner is a JS type; not whether the owner
   *  is a JS type.
   */
  def isJSDefaultParam(sym: Symbol)(implicit ctx: Context): Boolean = {
    sym.name.is(DefaultGetterName) && {
      val owner = sym.owner
      if (owner.is(ModuleClass)) {
        val isConstructor = sym.name match {
          case DefaultGetterName(methName, _) => methName == nme.CONSTRUCTOR
          case _ => false
        }
        if (isConstructor)
          isJSType(owner.linkedClass)
        else
          isJSType(owner)
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
