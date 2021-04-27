package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Flags._
import Symbols._
import NameOps._
import StdNames._
import Phases._

import dotty.tools.dotc.transform.sjs.JSSymUtils._

/** Management of the interoperability with JavaScript.
 *
 *  This object only contains forwarders for extension methods in
 *  `transform.sjs.JSSymUtils`. They are kept to minimize changes in
 *  `JSCodeGen` in the short term, but it will eventually be removed.
 */
object JSInterop {

  /** Is this symbol a JavaScript type? */
  def isJSType(sym: Symbol)(using Context): Boolean =
    sym.isJSType

  /** Should this symbol be translated into a JS getter?
   *
   *  This is true for any parameterless method, i.e., defined without `()`.
   *  Unlike `SymDenotations.isGetter`, it applies to user-defined methods as
   *  much as *accessor* methods created for `val`s and `var`s.
   */
  def isJSGetter(sym: Symbol)(using Context): Boolean =
    sym.isJSGetter

  /** Should this symbol be translated into a JS setter?
   *
   *  This is true for any method whose name ends in `_=`.
   *  Unlike `SymDenotations.isGetter`, it applies to user-defined methods as
   *  much as *accessor* methods created for `var`s.
   */
  def isJSSetter(sym: Symbol)(using Context): Boolean =
    sym.isJSSetter

  /** Should this symbol be translated into a JS bracket access?
   *
   *  This is true for methods annotated with `@JSBracketAccess`.
   */
  def isJSBracketAccess(sym: Symbol)(using Context): Boolean =
    sym.isJSBracketAccess

  /** Should this symbol be translated into a JS bracket call?
   *
   *  This is true for methods annotated with `@JSBracketCall`.
   */
  def isJSBracketCall(sym: Symbol)(using Context): Boolean =
    sym.isJSBracketCall

  /** Is this symbol a default param accessor for a JS method?
   *
   *  For default param accessors of *constructors*, we need to test whether
   *  the companion *class* of the owner is a JS type; not whether the owner
   *  is a JS type.
   */
  def isJSDefaultParam(sym: Symbol)(using Context): Boolean =
    sym.isJSDefaultParam

  /** Gets the unqualified JS name of a symbol.
   *
   *  If it is not explicitly specified with an `@JSName` annotation, the
   *  JS name is inferred from the Scala name.
   */
  def jsNameOf(sym: Symbol)(using Context): JSName =
    sym.jsName

}
