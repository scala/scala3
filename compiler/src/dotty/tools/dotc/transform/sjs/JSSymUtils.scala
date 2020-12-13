package dotty.tools.dotc
package transform
package sjs

import core._
import util.SrcPos
import Annotations._
import Constants._
import Contexts._
import Decorators._
import DenotTransformers._
import Flags._
import NameKinds.DefaultGetterName
import NameOps._
import Names._
import Phases._
import Scopes._
import StdNames._
import Symbols._
import SymDenotations._
import SymUtils._
import ast.Trees._
import Types._

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

/** Additional extensions for `Symbol`s that are only relevant for Scala.js. */
object JSSymUtils {
  /** The result type for `sym.jsName`.
   *
   *  It is either a literal string, or a computed name represented by a reference
   *  to a static `Symbol` (a `dotc.core.Symbols.Symbol`, not a `js.Symbol`).
   */
  enum JSName {
    case Literal(name: String)
    case Computed(sym: Symbol)

    def displayName(using Context): String = this match {
      case Literal(name) => name
      case Computed(sym) => sym.fullName.toString()
    }
  }

  extension (sym: Symbol) {
    /** Is this symbol a JavaScript type? */
    def isJSType(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSTypeAnnot)

    /** Is this symbol a non-native JS class? */
    def isNonNativeJSClass(using Context): Boolean =
      sym.isJSType && !sym.hasAnnotation(jsdefn.JSNativeAnnot)

    /** Is this symbol a nested JS class, i.e., an inner or local JS class? */
    def isNestedJSClass(using Context): Boolean =
      !sym.isStatic && sym.isJSType

    /** Tests whether the given member is exposed, i.e., whether it was
     *  originally a public or protected member of a non-native JS class.
     */
    def isJSExposed(using Context): Boolean = {
      !sym.is(Bridge) && {
        sym.hasAnnotation(jsdefn.ExposedJSMemberAnnot)
          || (sym.is(Accessor) && sym.field.hasAnnotation(jsdefn.ExposedJSMemberAnnot))
      }
    }

    /** Should this symbol be translated into a JS getter? */
    def isJSGetter(using Context): Boolean = {
      sym.is(Module) || (
          sym.is(Method)
            && sym.info.firstParamTypes.isEmpty
            && atPhaseNoLater(erasurePhase)(sym.info.isParameterless))
    }

    /** Should this symbol be translated into a JS setter? */
    def isJSSetter(using Context): Boolean =
      sym.originalName.isSetterName && sym.is(Method)

    /** Is this symbol a JS getter or setter? */
    def isJSProperty(using Context): Boolean =
      sym.isJSGetter || sym.isJSSetter

    /** Should this symbol be translated into a JS bracket access? */
    def isJSBracketAccess(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSBracketAccessAnnot)

    /** Should this symbol be translated into a JS bracket call? */
    def isJSBracketCall(using Context): Boolean =
      sym.hasAnnotation(jsdefn.JSBracketCallAnnot)

    /** Is this symbol a default param accessor for a JS method?
     *
     *  For default param accessors of *constructors*, we need to test whether
     *  the companion *class* of the owner is a JS type; not whether the owner
     *  is a JS type.
     */
    def isJSDefaultParam(using Context): Boolean = {
      sym.name.is(DefaultGetterName) && {
        val owner = sym.owner
        val methName = sym.name.exclude(DefaultGetterName)
        if (methName == nme.CONSTRUCTOR) {
          owner.linkedClass.isJSType
        } else {
          def isAttachedMethodExposed: Boolean =
            owner.info.decl(methName).hasAltWith(_.symbol.isJSExposed)
          owner.isJSType && (!owner.isNonNativeJSClass || isAttachedMethodExposed)
        }
      }
    }

    /** Gets the unqualified JS name of the symbol.
     *
     *  If it is not explicitly specified with an `@JSName` annotation, the
     *  JS name is inferred from the Scala name.
     */
    def jsName(using Context): JSName = {
      sym.getAnnotation(jsdefn.JSNameAnnot).fold[JSName] {
        JSName.Literal(defaultJSName)
      } { annotation =>
        annotation.arguments.head match {
          case Literal(Constant(name: String)) => JSName.Literal(name)
          case tree                            => JSName.Computed(tree.symbol)
        }
      }
    }

    def defaultJSName(using Context): String =
      if (sym.isTerm) sym.asTerm.name.unexpandedName.getterName.toString()
      else sym.name.unexpandedName.stripModuleClassSuffix.toString()
  }
}
