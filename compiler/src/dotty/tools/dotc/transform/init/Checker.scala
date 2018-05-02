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
      s"""|Initialization too late: $sym may be used during parent initialization.
          |Consider make it a class parameter."""
        .stripMargin

    for (decl <- cls.info.decls.toList if decl.is(AnyFlags, butNot = Method | Deferred)) {
      if (!decl.is(ParamAccessor | Override) && decl.isOverride)
        ctx.warning(lateInitMsg(decl), decl.pos)
    }

    var membersToCheck: util.SimpleIdentityMap[Name, Type] = util.SimpleIdentityMap.Empty[Name]
    val seenClasses = new util.HashSet[Symbol](256)

    def parents(cls: Symbol) =
      cls.info.parents.map(_.classSymbol)
        .filter(_.is(AbstractOrTrait))
        .dropWhile(_.is(JavaDefined | Scala2x))

    def addDecls(cls: Symbol): Unit =
      if (!seenClasses.contains(cls)) {
        seenClasses.addEntry(cls)
        for (mbr <- cls.info.decls)
          if (mbr.isTerm && mbr.is(Deferred | Method) &&
              (mbr.hasAnnotation(defn.PartialAnnot) || mbr.hasAnnotation(defn.FilledAnnot)) &&
              !membersToCheck.contains(mbr.name))
            membersToCheck = membersToCheck.updated(mbr.name, mbr.info.asSeenFrom(self, mbr.owner))
          parents(cls).foreach(addDecls)
      }
    parents(cls).foreach(addDecls)  // no need to check methods defined in current class

    def invalidImplementMsg(sym: Symbol) =
      s"""|@scala.annotation.partial required for ${sym.show} in ${sym.owner.show}
          |Because the abstract method it implements is marked as `@partial` or `@filled`."""
        .stripMargin

    for (name <- membersToCheck.keys) {
      val tp  = membersToCheck(name)
      for {
        mbrd <- self.member(name).alternatives
        if mbrd.info.overrides(tp, matchLoosely = true)
      } {
        val mbr = mbrd.symbol
        if (mbr.owner.ne(cls) &&
            !mbr.isOverride &&
            !mbr.hasAnnotation(defn.PartialAnnot) &&
            !mbr.hasAnnotation(defn.FilledAnnot) )
          ctx.warning(invalidImplementMsg(mbr), cls.pos)
      }
    }

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
    val obj = new ObjectValue(tp = cls.typeRef, open = !cls.is(Final) && !cls.isAnonymousClass)
      // enhancement possible to check if there are actual children
      // and whether children are possible in other modules.

    // for recursive usage
    root.addClassDef(cls, tmpl)
    indexOuter(cls, root)

    // init check
    val constr = tmpl.constr
    val values = constr.vparamss.flatten.map { param => param.tpe.widen.value }
    val poss = constr.vparamss.flatten.map(_.pos)
    val res = root.init(constr.symbol, values, poss, cls.pos, obj, analyzer)

    val sliceValue = obj.slices(cls).asInstanceOf[SliceValue]
    val slice = root.heap(sliceValue.id).asSlice

    res.effects.foreach(_.report)
    slice.notAssigned.foreach { sym =>
      if (!sym.is(Deferred)) ctx.warning(s"field ${sym.name} is not initialized", sym.pos)
    }

    // filled check: try commit early
    if (obj.open || slice.widen != FullValue) filledCheck(obj, tmpl, root.heap)
  }

  def partialCheck(cls: ClassSymbol, tmpl: tpd.Template, analyzer: Analyzer)(implicit ctx: Context) = {
    val obj = new ObjectValue(tp = cls.typeRef, open = !cls.is(Final) && !cls.isAnonymousClass)
      // enhancement possible to check if there are actual children
      // and whether children are possible in other modules.

    val root = Heap.createRootEnv
    val heap = root.heap
    val slice = root.newSlice(cls)
    analyzer.indexMembers(tmpl.body, slice)
    slice.innerEnv.add(cls, obj)
    indexOuter(cls, root)

    obj.add(cls, new SliceValue(slice.id))
    cls.baseClasses.tail.foreach(base => obj.add(base, PartialValue))

    if (!cls.is(Trait))
      tmpl.constr.vparamss.flatten.zipWithIndex.foreach { case (param: ValDef, index) =>
        val sym = cls.info.member(param.name).suchThat(x => !x.is(Method)).symbol
        if (sym.exists) slice.add(sym, sym.info.value)
      }

    def checkMethod(sym: Symbol): Unit = {
      if (!sym.isPartial && !sym.isOverride) return

      val heap2 = heap.clone
      var res = obj.select(sym, heap2, sym.pos)
      if (!sym.info.isParameterless)
        res = res.value.apply(i => FullValue, i => NoPosition, sym.pos, heap)

      if (res.hasErrors) {
        ctx.warning("Calling the partial method causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (res.value != FullValue) {
        ctx.warning("Partial method must return a full value", sym.pos)
      }
    }

    def checkLazy(sym: Symbol): Unit = {
      if (!sym.isPartial && !sym.isOverride) return

      val heap2 = heap.clone
      val res = obj.select(sym, heap2, sym.pos)
      if (res.hasErrors) {
        ctx.warning("Forcing partial lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else {
        val value = res.value.widen(heap2, sym.pos)
        if (value != FullValue) ctx.warning("Partial lazy value must return a full value", sym.pos)
      }
    }

    tmpl.body.foreach {
      case ddef: DefDef if !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef.symbol)
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef.symbol)
      case _ =>
    }
  }

  def filledCheck(obj: ObjectValue, tmpl: tpd.Template, heap: Heap)(implicit ctx: Context) = {
    def checkMethod(sym: Symbol): Unit = {
      if (sym.isPartial || sym.isOverride || !sym.isFilled) return

      var res = obj.select(sym, heap, sym.pos)
      if (!sym.info.isParameterless)
        res = res.value.apply(i => FullValue, i => NoPosition, sym.pos, heap)
      if (res.hasErrors) {
        ctx.warning("Calling the filled method causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else if (res.value != FullValue) {
        ctx.warning("Filled method must return a full value", sym.pos)
      }
    }

    def checkLazy(sym: Symbol): Unit = {
      if (sym.isPartial || sym.isOverride || !sym.isFilled) return

      val res = obj.select(sym, heap, sym.pos)
      if (res.hasErrors) {
        ctx.warning("Forcing filled lazy value causes errors", sym.pos)
        res.effects.foreach(_.report)
      }
      else {
        val value = res.value.widen(heap, sym.pos)
        if (value != FullValue) ctx.warning("Filled lazy value must return a full value", sym.pos)
      }
    }

    def checkValDef(sym: Symbol): Unit = {
      val isOverride = sym.allOverriddenSymbols.exists(sym => sym.isInit)
      val expected: OpaqueValue =
        if (isOverride) FullValue
        else sym.info.value.join(sym.value)

      val actual = obj.select(sym, heap, sym.pos).value.widen(heap, sym.pos)
      if (actual < expected) ctx.warning(s"Found = $actual, expected = $expected" , sym.pos)
    }

    tmpl.body.foreach {
      case ddef: DefDef if ddef.symbol.isFilled && !ddef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkMethod(ddef.symbol)
      case vdef: ValDef if vdef.symbol.is(Lazy)  =>
        checkLazy(vdef.symbol)
      case vdef: ValDef if !vdef.symbol.hasAnnotation(defn.UncheckedAnnot) =>
        checkValDef(vdef.symbol)
      case _ =>
    }
  }


  def indexOuter(cls: ClassSymbol, env: Env)(implicit ctx: Context) = {
    def recur(cls: Symbol, maxValue: OpaqueValue): Unit = if (cls.owner.exists) {
      val outerValue = cls.value
      val enclosingCls = cls.owner.enclosingClass

      if (!cls.owner.isClass || maxValue == FullValue) {
        env.add(enclosingCls, FullValue)
        recur(enclosingCls, FullValue)
      }
      else {
        val meet = outerValue.meet(maxValue)
        env.add(enclosingCls, meet)
        recur(enclosingCls, meet)
      }
    }
    recur(cls, cls.value)
  }
}
