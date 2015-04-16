package dotty.tools.dotc
package transform

import core._
import Types._
import Symbols._
import SymDenotations._
import Contexts._
import Flags._
import StdNames._

/** Methods that apply to user-defined value classes */
object ValueClasses {

  def isDerivedValueClass(d: SymDenotation)(implicit ctx: Context) = {
    d.isValueClass &&
    (d.initial.symbol ne defn.AnyValClass) && // Compare the initial symbol because AnyVal does not exist after erasure
    !d.isPrimitiveValueClass
  }

  def isMethodWithExtension(d: SymDenotation)(implicit ctx: Context) =
    d.isSourceMethod &&
      isDerivedValueClass(d.owner) &&
      !d.isConstructor &&
      !d.is(SuperAccessor) &&
      !d.is(Macro) &&
      !(d.name eq nme.COMPANION_MODULE_METHOD)

  /** The member that of a derived value class that unboxes it. */
  def valueClassUnbox(d: ClassDenotation)(implicit ctx: Context): Symbol =
    // (info.decl(nme.unbox)).orElse(...)      uncomment once we accept unbox methods
    d.classInfo.decls
      .find(d => d.isTerm && d.symbol.is(ParamAccessor))
      .map(_.symbol)
      .getOrElse(NoSymbol)

  /** The unboxed type that underlies a derived value class */
  def underlyingOfValueClass(d: ClassDenotation)(implicit ctx: Context): Type =
    valueClassUnbox(d).info.resultType

}
