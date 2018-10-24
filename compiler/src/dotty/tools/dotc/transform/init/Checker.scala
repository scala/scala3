package dotty.tools.dotc
package transform
package init

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
import DenotTransformers._
import util.Positions._
import config.Printers.init.{ println => debug }
import Constants.Constant
import collection.mutable

object Checker {
  val name = "initChecker"
}

/** This transform checks initialization is safe based on data-flow analysis
 *
 *  - Partial
 *  - Filled
 *  - Full
 *
 *  1. A _full_ object is fully initialized.
 *  2. All fields of a _filled_ object are assigned, but the fields may refer to non-full objects.
 *  3. A _partial_ object may have unassigned fields.
 *
 *  TODO:
 *   - check default arguments of init methods
 *   - selection on ParamAccessors of partial value is fine if the param is not partial
 *   - handle tailrec calls during initialization (which captures `this`)
 */
class Checker extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = Checker.name

  override def transformTemplate(tree: Template)(implicit ctx: Context): Tree = {
    val cls = ctx.owner.asClass
    val self = cls.thisType

    // ignore init checking if `@unchecked`
    if (cls.hasAnnotation(defn.UncheckedAnnot)) return tree

    def lateInitMsg(sym: Symbol) =
      s"""|Initialization too late: $sym is used during parent initialization.
          |Consider make it a class parameter."""
        .stripMargin

    for (decl <- cls.info.decls.toList if decl.is(AnyFlags, butNot = Method | Deferred)) {
      if (!decl.is(ParamAccessor | Override) && decl.isCalledAbove(cls))
        ctx.warning(lateInitMsg(decl), decl.pos)
    }

    def invalidImplementMsg(sym: Symbol) = {
      val annot = if (sym.owner.is(Trait)) "partial" else "init"
      s"""|@scala.annotation.$annot required for ${sym.show} in ${sym.owner.show}
          |Because the method is called during initialization."""
        .stripMargin
    }

    def parents(cls: ClassSymbol) =
      cls.baseClasses.tail.filter(_.is(AbstractOrTrait)).dropWhile(_.is(JavaDefined | Scala2x))

    def check(curCls: ClassSymbol): Unit = {
      for {
        mbr <- calledSymsIn(curCls)
        mbrd <- self.member(mbr.name).alternatives
        tp = mbr.info.asSeenFrom(self, mbr.owner)
        if mbrd.info.overrides(tp, matchLoosely = true) &&
           !mbrd.symbol.isInit && !mbrd.symbol.isPartial &
           !mbrd.symbol.isCalledAbove(cls.asClass) &&
           !mbrd.symbol.is(Deferred)
      } ctx.warning(invalidImplementMsg(mbrd.symbol), cls.pos)
    }
    parents(cls).foreach(check)  // no need to check methods defined in current class

    checkInit(cls, tree)

    tree
  }

  def checkInit(cls: ClassSymbol, tmpl: tpd.Template)(implicit ctx: Context) = {
    debug("*************************************")
    debug("checking " + cls.show)
    debug("*************************************")

    val analyzer = new Analyzer

    // partial check
    partialCheck(cls, tmpl, analyzer)

    // current class env needs special setup
    val root = Heap.createRootEnv
    val setting = Setting(root, cls.pos, ctx, analyzer)
    val obj = new ObjectValue(tp = cls.typeRef, open = !cls.is(Final) && !cls.isAnonymousClass)
      // enhancement possible to check if there are actual children
      // and whether children are possible in other modules.

    // for recursive usage
    root.addClassDef(cls, tmpl)
    indexOuter(cls)(setting)

    // init check
    val constr = tmpl.constr
    val values = constr.vparamss.flatten.map { param => param.tpe.widen.value }
    val poss = constr.vparamss.flatten.map(_.pos)
    val res = root.init(constr.symbol, values, poss, obj)(setting)

    val sliceValue = obj.slices(cls).asInstanceOf[SliceValue]
    val slice = root.heap(sliceValue.id).asSlice

    res.effects.foreach(_.report)

    if (obj.open) obj.annotate(cls)

    // init check: try commit early
    if (obj.open) initCheck(cls, obj, tmpl)(setting)
  }

  def partialCheck(cls: ClassSymbol, tmpl: tpd.Template, analyzer: Analyzer)(implicit ctx: Context) = {
    val obj = new ObjectValue(tp = cls.typeRef, open = !cls.is(Final) && !cls.isAnonymousClass)
      // enhancement possible to check if there are actual children
      // and whether children are possible in other modules.

    def checkMethod(ddef: tpd.DefDef): Unit = {
      val sym = ddef.symbol
      if (!sym.isEffectivePartial) return

      val root = Heap.createRootEnv
      val setting: Setting = Setting(root, sym.pos, ctx, analyzer)
      indexOuter(cls)(setting)
      if (sym.isPartial) root.add(cls, BlankValue)
      else root.add(cls, PartialValue)

      val value = analyzer.methodValue(ddef)(setting.strict)
      val res = value.apply(i => FullValue, i => NoPosition)(setting.strict)

      if (res.hasErrors) {
        ctx.warning("Calling the method during initialization causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (res.value != FullValue) {
        ctx.warning("A method called during initialization must return a fully initialized value", sym.pos)
      }
    }

    def checkLazy(vdef: tpd.ValDef): Unit = {
      val sym = vdef.symbol
      if (!sym.isEffectivePartial) return

      val root = Heap.createRootEnv
      val setting: Setting = Setting(root, sym.pos, ctx, analyzer)
      indexOuter(cls)(setting)
      if (sym.isPartial) root.add(cls, BlankValue)
      else root.add(cls, PartialValue)

      val value = analyzer.lazyValue(vdef)(setting.strict)
      val res = value.apply(i => FullValue, i => NoPosition)(setting.strict)

      if (res.hasErrors) {
        ctx.warning("Forcing partial lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else {
        val value = res.value.widen()(setting.strict)
        if (value != FullValue) ctx.warning("Partial lazy value must return a full value", sym.pos)
      }
    }

    tmpl.body.foreach {
      case ddef: DefDef if !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef)
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef)
      case _ =>
    }
  }

  def initCheck(cls: ClassSymbol, obj: ObjectValue, tmpl: tpd.Template)(implicit setting: Setting) = {
    def checkMethod(sym: Symbol): Unit = {
      if (!sym.isEffectiveInit) return

      var res = obj.select(sym, isStaticDispatch = true)(setting.withPos(sym.pos))
      if (!sym.info.isParameterless)
        res = res.value.apply(i => FullValue, i => NoPosition)
      if (res.hasErrors) {
        setting.ctx.warning("Calling the init method causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (res.value != FullValue) {
        setting.ctx.warning("An init method must return a full value", sym.pos)
      }

      obj.clearDynamicCalls()
    }

    def checkLazy(sym: Symbol): Unit = {
      if (!sym.isEffectiveInit) return

      val res = obj.select(sym, isStaticDispatch = true)(setting.withPos(sym.pos))
      if (res.hasErrors) {
        setting.ctx.warning("Forcing init lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else {
        val value = res.value.widen()(setting.strict)
        if (value != FullValue) setting.ctx.warning("Init lazy value must return a full value", sym.pos)
      }

      obj.clearDynamicCalls()
    }

    def checkValDef(sym: Symbol): Unit = {
      if (sym.is(Flags.PrivateOrLocal)) return

      val actual = obj.select(sym, isStaticDispatch = true)(setting.withPos(sym.pos)).value.widen()(setting.strict)
      if (actual < FullValue) sym.addAnnotation(Annotations.ConcreteAnnotation(New(defn.FilledAnnotType, Nil)))

      obj.clearDynamicCalls()
    }

    tmpl.body.foreach {
      case ddef: DefDef if ddef.symbol.isInit && !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef.symbol)
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef.symbol)
      case vdef: ValDef =>
        checkValDef(vdef.symbol)
      case _ =>
    }
  }


  def indexOuter(cls: ClassSymbol)(implicit setting: Setting) = {
    def recur(cls: Symbol, maxValue: OpaqueValue): Unit = if (cls.owner.exists) {
      val outerValue = cls.value
      val enclosingCls = cls.owner.enclosingClass

      if (!cls.owner.isClass || maxValue == FullValue) {
        setting.env.add(enclosingCls, FullValue)
        recur(enclosingCls, FullValue)
      }
      else {
        val meet = outerValue.meet(maxValue)
        setting.env.add(enclosingCls, meet)
        recur(enclosingCls, meet)
      }
    }
    recur(cls, cls.value)
  }
}
