package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import StdNames._
import Names._
import Phases._
import ast._
import Trees._
import Flags._
import SymUtils._
import Symbols._
import Denotations._
import SymDenotations._
import Types._
import Decorators._
import util.Positions._
import Constants.Constant
import collection.mutable

package object init {
  implicit class TypeOps(val tp: Type) extends AnyVal {
    def isPartial(implicit ctx: Context) = tp.dealiasKeepAnnots.hasAnnotation(defn.PartialAnnot)
    def isFilled(implicit ctx: Context) = tp.dealiasKeepAnnots.hasAnnotation(defn.FilledAnnot)

    def value(implicit ctx: Context) =
      if (isPartial) PartialValue
      else if (isFilled) FilledValue
      else FullValue
  }

  implicit class SymOps(val sym: Symbol) extends AnyVal {
    def isPartial(implicit ctx: Context) = sym.hasAnnotation(defn.PartialAnnot)
    def isFilled(implicit ctx: Context) = sym.hasAnnotation(defn.FilledAnnot)
    def isInit(implicit ctx: Context) = sym.hasAnnotation(defn.InitAnnot)
    def isOverride(implicit ctx: Context) =
      (sym.is(Method) && sym.allOverriddenSymbols.exists(sym => sym.isPartial || sym.isFilled)) ||
      (!sym.is(Method) && sym.allOverriddenSymbols.exists(sym => sym.isPartial || sym.isFilled || sym.isInit))

    def isPrimaryConstructorFields(implicit ctx: Context) = sym.is(ParamAccessor)

    def isDefinedOn(tp: Type)(implicit ctx: Context): Boolean =
      tp.classSymbol.isSubClass(sym.owner)

    def value(implicit ctx: Context) =
      if (isPartial) PartialValue
      else if (isFilled) FilledValue
      else FullValue

    def isConcreteField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Deferred | Method | Local | Private)

    def isNonParamField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Method | ParamAccessor | Lazy | Deferred)

    def isField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Method | Lazy | Deferred)
  }
}