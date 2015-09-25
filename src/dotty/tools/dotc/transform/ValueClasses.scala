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
    !ctx.settings.XnoValueClasses.value &&
    !d.isRefinementClass &&
    d.isValueClass &&
    (d.initial.symbol ne defn.AnyValClass) && // Compare the initial symbol because AnyVal does not exist after erasure
    !d.isPrimitiveValueClass
  }

  def isMethodWithExtension(d: SymDenotation)(implicit ctx: Context) =
    d.isRealMethod &&
      !(d.initial.validFor.firstPhaseId > ctx.extensionMethodsPhase.id) &&
      isDerivedValueClass(d.owner) &&
      !d.isConstructor &&
      !d.is(SuperAccessor) &&
      !d.is(Macro)

  /** The member that of a derived value class that unboxes it. */
  def valueClassUnbox(d: ClassDenotation)(implicit ctx: Context): Symbol =
    // (info.decl(nme.unbox)).orElse(...)      uncomment once we accept unbox methods
    d.classInfo.decls
      .find(d => d.isTerm && d.symbol.is(ParamAccessor))
      .map(_.symbol)
      .getOrElse(NoSymbol)

  /** For a value class `d`, this returns the synthetic cast from the underlying type to
   *  ErasedValueType defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def u2evt(d: ClassDenotation)(implicit ctx: Context): Symbol =
    d.linkedClass.info.decl(nme.U2EVT).symbol

  /** For a value class `d`, this returns the synthetic cast from ErasedValueType to the
   *  underlying type defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def evt2u(d: ClassDenotation)(implicit ctx: Context): Symbol =
    d.linkedClass.info.decl(nme.EVT2U).symbol

  /** The unboxed type that underlies a derived value class */
  def underlyingOfValueClass(d: ClassDenotation)(implicit ctx: Context): Type =
    valueClassUnbox(d).info.resultType

}
