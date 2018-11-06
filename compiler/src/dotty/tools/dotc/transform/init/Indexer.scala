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


trait Indexer { self: Analyzer =>
  import tpd._

  def methodValue(ddef: DefDef)(implicit setting: Setting): FunctionValue =
    new FunctionValue { fv =>
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting2: Setting): Res = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        if (isChecking(ddef.symbol)) {
          // TODO: check if fixed point has reached. But the domain is infinite, thus non-terminating.
          debug(s"recursive call of ${ddef.symbol} found")
          Res()
        }
        else {
          val env2 = setting.env.fresh(setting2.heap)
          val setting3 = setting2.withCtx(setting2.ctx.withOwner(ddef.symbol)).withEnv(env2)

          ddef.vparamss.flatten.zipWithIndex.foreach { case (param: ValDef, index) =>
            env2.add(param.symbol, value = values(index))
          }
          val res = checking(ddef.symbol) { self.apply(ddef.rhs)(setting3) }
          if (res.hasErrors) res.effects = Vector(Call(ddef.symbol, res.effects, setting2.pos))
          res
        }
      }

      def widen(implicit setting2: Setting) = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        val env = setting2.heap(setting.env.id).asEnv
        val setting3 = setting2.withCtx(setting2.ctx.withOwner(ddef.symbol)).withEnv(env)
        setting3.widenFor(fv) { widenTree(ddef)(setting3) }
      }

      override def show(implicit setting: ShowSetting): String = ddef.symbol(setting.ctx).show(setting.ctx)
    }

  def classValue(cdef: TypeDef)(implicit setting: Setting): ClassValue =
    new ClassValue { cv =>
      def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting2: Setting): Res = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        if (isChecking(cdef.symbol)) {
          // TODO: check if fixed point has reached. But the domain is infinite, thus non-terminating.
          debug(s"recursive instantiation of ${cdef.symbol} found")
          Res()
        }
        else {
          val tmpl = cdef.rhs.asInstanceOf[Template]
          val env2 = setting.env.fresh(setting2.heap)
          val setting3 = setting2.withCtx(setting2.ctx.withOwner(cdef.symbol)).withEnv(env2)
          self.init(constr, tmpl, values, argPos, obj)(setting3)
        }
      }

      def widen(implicit setting2: Setting) = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        val env = setting2.heap(setting.env.id).asEnv
        val setting3 = setting2.withCtx(setting2.ctx.withOwner(cdef.symbol)).withEnv(env)
        setting3.widenFor(cv) { widenTree(cdef)(setting3) }
      }

      override def show(implicit setting: ShowSetting): String = cdef.symbol(setting.ctx).show(setting.ctx)
    }

  def widenTree(tree: Tree)(implicit setting: Setting): OpaqueValue = {
    val captured = Capture.analyze(tree)
    indentedDebug(s"captured in ${tree.symbol}: " + captured.keys.map(_.show).mkString(", "))

    val value = WarmValue(captured.keys.toSet, unknownDeps = false).widen
    if (!value.isHot) indentedDebug(s"not hot in ${tree.symbol}: " + value.show(setting.showSetting))
    value
  }

  def lazyValue(vdef: ValDef)(implicit setting: Setting): LazyValue =
    new LazyValue { lz =>
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting2: Setting): Res = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        if (isChecking(vdef.symbol)) {
          // TODO: check if fixed point has reached. But the domain is infinite, thus non-terminating.
          debug(s"recursive call of ${vdef.symbol} found")
          Res()
        }
        else {
          val env2 = setting2.heap(setting.env.id).asEnv
          val setting3: Setting = setting2.withCtx(setting2.ctx.withOwner(vdef.symbol)).withEnv(env2)
          val res = checking(vdef.symbol) { self.apply(vdef.rhs)(setting3) }
          if (res.hasErrors) res.effects = Vector(Force(vdef.symbol, res.effects, setting2.pos))
          res
        }
      }

      def widen(implicit setting2: Setting) = {
        // TODO: implicit ambiguities
        implicit val ctx: Context = setting2.ctx
        val env = setting2.heap(setting.env.id).asEnv
        val setting3 = setting2.withCtx(setting2.ctx.withOwner(vdef.symbol)).withEnv(env)
        setting3.widenFor(lz) { widenTree(vdef)(setting3) }
      }

      override def show(implicit setting: ShowSetting): String = vdef.symbol(setting.ctx).show(setting.ctx)
    }

  /** Index local definitions */
  def indexStats(stats: List[Tree])(implicit setting: Setting): Unit = stats.foreach {
    case ddef: DefDef if !ddef.symbol.isConstructor =>  // TODO: handle secondary constructor
      setting.env.add(ddef.symbol, methodValue(ddef))
    case vdef: ValDef if vdef.symbol.is(Lazy)  =>
      setting.env.add(vdef.symbol, lazyValue(vdef))
    case vdef: ValDef =>
      setting.env.add(vdef.symbol, NoValue)
    case tdef: TypeDef if tdef.isClassDef  =>
      setting.env.add(tdef.symbol.asClass, classValue(tdef))
    case _ =>
  }

  /** Index member definitions
   *
   *  trick: use `slice` for name resolution, but `env` for method execution
   */
  def indexMembers(stats: List[Tree], slice: SliceRep)(implicit setting: Setting): Unit = stats.foreach {
    case ddef: DefDef =>
      slice.add(ddef.symbol, methodValue(ddef)(setting.withEnv(slice.innerEnv)))
    case vdef: ValDef if vdef.symbol.is(Lazy)  =>
      slice.add(vdef.symbol, lazyValue(vdef)(setting.withEnv(slice.innerEnv)))
    case vdef: ValDef =>
      val value = if (vdef.symbol.isInit || vdef.symbol.is(Deferred)) HotValue else NoValue
      slice.add(vdef.symbol, value)
    case tdef: TypeDef if tdef.isClassDef  =>
      slice.add(tdef.symbol.asClass, classValue(tdef)(setting.withEnv(slice.innerEnv)))
    case _ =>
  }

  def indexOuter(cls: ClassSymbol)(implicit setting: Setting) = {
    def recur(cls: ClassSymbol, maxValue: OpaqueValue): Unit = if (cls.owner.exists) {
      val outerValue = cls.value
      val enclosingCls = cls.owner.enclosingClass

      if (!enclosingCls.exists) return
      else if (maxValue.isHot || outerValue.isHot) {
        setting.env.add(enclosingCls, HotValue)
        recur(enclosingCls.asClass, HotValue)
      }
      else {
        val cls = enclosingCls.asClass
        val open = !cls.is(Final) && !cls.isAnonymousClass
        val obj = new ObjectValue(cls.thisType, open = open)
        obj.add(cls, outerValue)
        setting.env.add(cls, obj)
        recur(cls, outerValue)
      }
    }
    recur(cls, cls.value)
  }

  def init(constr: Symbol, tmpl: Template, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val cls = constr.owner.asClass

    if (isChecking(cls)) {
      debug(s"recursive creation of $cls found")
      Res()
    }
    else checking(cls) {
      val slice = setting.env.newSlice(cls)
      obj.add(cls, new SliceValue(slice.id))

      // cold check
      coldCheck(cls, tmpl)(setting.widening)

      // The outer of parents are set (but not recursively)
      // before any super-calls if they are known.
      // This is not specified in the Scala specification.
      // Calling methods of an unrelated trait during initialization
      // is dangerous, thus should be discouraged. Therefore, the analyzer
      // doesn't follow closely the semantics here.

      // first index current class
      indexMembers(tmpl.body, slice)

      // propagate constructor arguments
      tmpl.constr.vparamss.flatten.zipWithIndex.foreach { case (param: ValDef, index) =>
        val sym = cls.info.member(param.name).suchThat(x => !x.is(Method)).symbol
        if (sym.exists) slice.add(sym, values(index))
        slice.innerEnv.add(param.symbol, values(index))
      }

      // setup this
      slice.innerEnv.add(cls, obj)

      // call parent constructor
      val setting2 = setting.withCtx(setting.ctx.withOwner(cls.owner)).withEnv(slice.innerEnv).withPos(cls.pos)
      val res = checkParents(cls, tmpl.parents, obj)(setting2)
      if (res.hasErrors) return res

      // check current class body
      res ++= checkStats(tmpl.body)(setting2).effects

      // init check: need to recheck for invariants @init, @cold to avoid verbose annotation
      initCheck(cls, obj, tmpl)(setting2.widening)

      res
    }
  }

  def coldCheck(cls: ClassSymbol, tmpl: tpd.Template)(implicit setting: Setting) = {
    def checkMethod(ddef: tpd.DefDef)(implicit setting: Setting): Unit = {
      val sym = ddef.symbol
      if (!sym.isEffectiveCold) return

      if (sym.isIcy) setting.env.add(cls, IcyValue)
      else setting.env.add(cls, ColdValue)

      val value = self.methodValue(ddef)(setting)
      val res = value.apply(i => HotValue, i => NoPosition)(setting)

      if (res.hasErrors) {
        setting.ctx.warning("Calling the method during initialization causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (!res.value.widen(setting).isHot) {
        setting.ctx.warning("A method called during initialization must return a fully initialized value", sym.pos)
      }
    }

    def checkLazy(vdef: tpd.ValDef)(implicit setting: Setting): Unit = {
      val sym = vdef.symbol
      if (!sym.isEffectiveCold || sym.is(Flags.Module)) return

      if (sym.isIcy) setting.env.add(cls, IcyValue)
      else setting.env.add(cls, ColdValue)

      val value = self.lazyValue(vdef)(setting)
      val res = value.apply(i => HotValue, i => NoPosition)(setting)

      if (res.hasErrors) {
        setting.ctx.warning("Forcing cold lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else {
        val value = res.value.widen(setting)
        if (!value.isHot) setting.ctx.warning("Cold lazy value must return a full value", sym.pos)
      }
    }

    tmpl.body.foreach {
      case ddef: DefDef if !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef)(setting.withPos(ddef.symbol.pos))
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef)(setting.withPos(vdef.symbol.pos))
      case _ =>
    }
  }

  def initCheck(cls: ClassSymbol, obj: ObjectValue, tmpl: tpd.Template)(implicit setting: Setting) = {
    def checkMethod(ddef: tpd.DefDef)(implicit setting: Setting): Unit = {
      val sym = ddef.symbol
      if (!sym.isEffectiveInit && !sym.isCalledIn(cls)) return

      var res = obj.select(sym, isStaticDispatch = true)
      res = res.value.apply(i => HotValue, i => NoPosition)
      if (res.hasErrors) {
        setting.ctx.warning(s"Calling the init $sym causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (!res.value.widen(setting).isHot) {
        setting.ctx.warning("A dynamic init method must return a full value", sym.pos)
      }
    }

    def checkLazy(vdef: tpd.ValDef)(implicit setting: Setting): Unit = {
      val sym = vdef.symbol
      val res = obj.select(sym, isStaticDispatch = true)

      if (res.hasErrors && sym.isEffectiveInit) {
        setting.ctx.warning("Forcing init lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (!res.hasErrors) {
        val value = res.value.widen(setting)
        if (!value.isHot && sym.isEffectiveInit) setting.ctx.warning("Init lazy value must return a full value", sym.pos)
        else if (value.isHot && !sym.isEffectiveInit) sym.annotate(defn.InitAnnotType)  // infer @init for lazy fields
      }

    }

    def checkValDef(vdef: tpd.ValDef)(implicit setting: Setting): Unit = {
      val sym = vdef.symbol
      if (sym.is(Flags.PrivateOrLocal)) return

      val actual = obj.select(sym, isStaticDispatch = true).value.widen(setting)
      setting.analyzer.indentedDebug(s"${sym.show} widens to ${actual.show(setting.showSetting)}")

      if (actual.isCold) sym.annotate(defn.ColdAnnotType)
      else if (actual.isWarm) sym.annotate(defn.WarmAnnotType)

      if (sym.isOverrideClassParam && !sym.isClassParam) {
        setting.ctx.warning("Overriding a class parameter in class body may cause initialization problems", sym.pos)
      }
      else if (!sym.isHot && sym.allOverriddenSymbols.exists(_.isHot)) {
        setting.ctx.warning("Overriding a fully initialized field with a cold value may cause initialization problems", sym.pos)
      }
    }

    def checkClassDef(cdef: tpd.TypeDef)(implicit setting: Setting): Unit = {
      val sym = cdef.symbol
      if (sym.isInit) {
        val setting2 = setting.widening
        val value = setting2.analyzer.widenTree(cdef)(setting2)

        val captured = Capture.analyze(cdef)(setting2)
        val notHot = captured.keys.filterNot(setting2.widen(_).isHot)

        for(key <- notHot; tree <- captured(key))
          setting.ctx.warning(s"The init $sym captures " + tree.show + ".\nTry to make captured fields or methods private or final.", tree.pos)
      }
      else {
        val classValue = obj.select(sym).value.widen(setting.widening)
        if (classValue.isHot) sym.annotate(defn.InitAnnotType)
      }
    }

    tmpl.body.foreach {
      case ddef: DefDef if !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef)(setting.withPos(ddef.symbol.pos))
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef)(setting.withPos(vdef.symbol.pos))
      case vdef: ValDef =>
        checkValDef(vdef)(setting.withPos(vdef.symbol.pos))
      case cdef: TypeDef if cdef.isClassDef =>
        checkClassDef(cdef)(setting.withPos(cdef.symbol.pos))
      case _ =>
    }
  }
}
