package dotty.tools.dotc
package transform

import core._
import Types._
import Symbols._
import Contexts._
import Flags._
import StdNames._
import SymUtils._

/** Methods that apply to user-defined value classes */
object ValueClasses {

  def isDerivedValueClass(sym: Symbol)(implicit ctx: Context): Boolean = {
    val d = sym.denot
    !ctx.settings.XnoValueClasses.value &&
    !d.isRefinementClass &&
    d.isValueClass &&
    (d.initial.symbol ne defn.AnyValClass) && // Compare the initial symbol because AnyVal does not exist after erasure
    !d.isPrimitiveValueClass
  }

  def isMethodWithExtension(sym: Symbol)(implicit ctx: Context): Boolean =
    ctx.atPhaseNotLaterThan(ctx.extensionMethodsPhase) {
      val d = sym.denot
      d.validFor.containsPhaseId(the[Context].phaseId) &&
      d.isRealMethod &&
      isDerivedValueClass(d.owner) &&
      !d.isConstructor &&
      !d.isSuperAccessor &&
      !d.is(Macro)
    }

  /** The member of a derived value class that unboxes it. */
  def valueClassUnbox(cls: ClassSymbol)(implicit ctx: Context): Symbol =
    // (info.decl(nme.unbox)).orElse(...)      uncomment once we accept unbox methods
    cls.classInfo.decls.find(_.is(ParamAccessor))

  /** For a value class `d`, this returns the synthetic cast from the underlying type to
   *  ErasedValueType defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def u2evt(cls: ClassSymbol)(implicit ctx: Context): Symbol =
    cls.linkedClass.info.decl(nme.U2EVT).symbol

  /** For a value class `d`, this returns the synthetic cast from ErasedValueType to the
   *  underlying type defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def evt2u(cls: ClassSymbol)(implicit ctx: Context): Symbol =
    cls.linkedClass.info.decl(nme.EVT2U).symbol

  /** The unboxed type that underlies a derived value class */
  def underlyingOfValueClass(sym: ClassSymbol)(implicit ctx: Context): Type =
    valueClassUnbox(sym).info.resultType

  /** Whether a value class wraps itself */
  def isCyclic(cls: ClassSymbol)(implicit ctx: Context): Boolean = {
    def recur(seen: Set[Symbol], clazz: ClassSymbol)(implicit ctx: Context): Boolean =
      (seen contains clazz) || {
        val unboxed = underlyingOfValueClass(clazz).typeSymbol
        (isDerivedValueClass(unboxed)) && recur(seen + clazz, unboxed.asClass)
      }

    recur(Set[Symbol](), cls)
  }
}
