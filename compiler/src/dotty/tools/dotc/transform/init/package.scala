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
import Annotations._
import util.Positions._
import Constants.Constant
import collection.mutable

package object init {
  implicit class TypeOps(val tp: Type) extends AnyVal {
    def isCold(implicit ctx: Context) = tp.dealiasKeepAnnots.hasAnnotation(defn.ColdAnnot)

    def isWarm(implicit ctx: Context) = tp.dealiasKeepAnnots.hasAnnotation(defn.WarmAnnot)

    def value(implicit ctx: Context) =
      if (isCold) ColdValue
      else if (isWarm) WarmValue()
      else HotValue
  }

  def calledSymsIn(cls: ClassSymbol)(implicit ctx: Context): List[Symbol] =
    cls.self.annotations.collect {
      case Annotation.Call(sym) => sym
    }

  implicit class SymOps(val sym: Symbol) extends AnyVal {
    def isCold(implicit ctx: Context) =
      sym.hasAnnotation(defn.ColdAnnot) || sym.info.isCold

    def isWarm(implicit ctx: Context) =
      sym.hasAnnotation(defn.WarmAnnot) || sym.info.isWarm

    def isInit(implicit ctx: Context) = sym.hasAnnotation(defn.InitAnnot)

    def isEffectiveCold(implicit ctx: Context) =
      sym.isCold || sym.isCalledAbove(sym.owner.asClass)

    def isEffectiveInit(implicit ctx: Context) =
      !sym.isEffectiveCold &&
      (sym.isInit || sym.allOverriddenSymbols.exists(_.isInit))

    def isCalledIn(cls: ClassSymbol)(implicit ctx: Context): Boolean =
      calledSymsIn(cls).exists(_ == sym) || sym.allOverriddenSymbols.exists(_.isCalledIn(cls))

    def isCalledAbove(from: ClassSymbol)(implicit ctx: Context) =
      from.baseClasses.tail.exists(cls => sym.isCalledIn(cls))

    def isClassParam(implicit ctx: Context) = sym.is(ParamAccessor)

    def isDefinedOn(tp: Type)(implicit ctx: Context): Boolean =
      tp.classSymbol.isSubClass(sym.owner)

    def value(implicit ctx: Context) =
      if (isCold) ColdValue
      else if (isWarm) WarmValue()
      else HotValue

    def isConcreteField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Deferred | Method | Local | Private)

    def isNonParamField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Method | ParamAccessor | Lazy | Deferred)

    def isField(implicit ctx: Context) =
      sym.isTerm && sym.is(AnyFlags, butNot = Method | Lazy | Deferred)

    def annotate(tp: Type)(implicit ctx: Context) =
      sym.addAnnotation(Annotations.ConcreteAnnotation(tpd.New(tp, Nil)))

    def enclosedIn(inSym: Symbol)(implicit ctx: Context): Boolean =
      sym.exists && ((sym `eq` inSym) || sym.owner.enclosedIn(inSym))
  }

  implicit def setting2ctx(implicit s: Setting): Context = s.ctx
  implicit def showSetting2ctx(implicit s: ShowSetting): Context = s.ctx
}