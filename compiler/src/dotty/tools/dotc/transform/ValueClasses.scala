package dotty.tools.dotc
package transform

import core.*
import Types.*
import Symbols.*
import Contexts.*
import Phases.*
import Flags.*
import StdNames.*

/** Methods that apply to user-defined value classes */
object ValueClasses {

  def isMethodWithExtension(sym: Symbol)(using Context): Boolean =
    val d = sym.denot.initial
    d.validFor.firstPhaseId <= extensionMethodsPhase.id
    && d.isRealMethod
    && isDerivedValueClass(d.owner)
    && !d.isConstructor
    && !d.symbol.isSuperAccessor
    && !d.isInlineMethod
    && !d.is(Macro)

  /** The member of a derived value class that unboxes it. */
  def valueClassUnbox(cls: ClassSymbol)(using Context): Symbol =
    // (info.decl(nme.unbox)).orElse(...)      uncomment once we accept unbox methods
    cls.classInfo.decls.find(_.is(ParamAccessor))

  /** For a value class `d`, this returns the synthetic cast from the underlying type to
   *  ErasedValueType defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def u2evt(cls: ClassSymbol)(using Context): Symbol =
    cls.linkedClass.info.decl(nme.U2EVT).symbol

  /** For a value class `d`, this returns the synthetic cast from ErasedValueType to the
   *  underlying type defined in the companion module. This method is added to the module
   *  and further described in [[ExtensionMethods]].
   */
  def evt2u(cls: ClassSymbol)(using Context): Symbol =
    cls.linkedClass.info.decl(nme.EVT2U).symbol

  /** The unboxed type that underlies a derived value class */
  def underlyingOfValueClass(sym: ClassSymbol)(using Context): Type =
    valueClassUnbox(sym).info.resultType

  /** The unboxed type that underlies a derived value class as seen from as seen from tpe*/
  def underlyingOfValueClass(sym: ClassSymbol, tpe: Type)(using Context): Type =
    valueClassUnbox(sym).info.resultType.asSeenFrom(tpe, sym)
  
}
