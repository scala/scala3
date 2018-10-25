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


class Analyzer extends Indexer { analyzer =>
  import tpd._

  var depth: Int = 0

  def trace(msg: => String)(body: => Res)(implicit setting: Setting) = {
    indentedDebug(s"==> ${pad(msg)}?")
    indentedDebug("heap = " + setting.heap.show)
    indentedDebug(setting.env.show(setting.showSetting))
    depth += 1
    val res = body
    depth -= 1
    indentedDebug(s"<== ${pad(msg)} = ${pad(res.show(setting.showSetting))}")
    res
  }

  def pad(s: String, padFirst: Boolean = false) = ShowSetting.pad(s, depth, padFirst)

  def indentedDebug(msg: => String) = debug(ShowSetting.pad(msg, depth, padFirst = true))

  def checkApply(tree: tpd.Tree, fun: Tree, argss: List[List[Tree]])(implicit setting: Setting): Res = {
    val funSym = fun.symbol
    val funRes = apply(fun)

    val args = argss.flatten
    val values = args.map { arg =>
      val res = apply(arg)
      funRes ++= res.effects
      res.value
    }

    indentedDebug(s">>> calling $funSym")
    funRes.value(values, args.map(_.pos)) ++ funRes.effects
  }

  def checkSelect(tree: Select)(implicit setting: Setting): Res = {
    val prefixRes = apply(tree.qualifier)
    val res = prefixRes.value.select(tree.symbol)
    res.effects = prefixRes.effects ++ res.effects
    res
  }

  def checkRef(tp: Type)(implicit setting: Setting): Res = trace("checking " + tp.show)(tp match {
    case tp : TermRef if tp.symbol.is(Module) && setting.ctx.owner.enclosedIn(tp.symbol.moduleClass) =>
      // self reference by name: object O { ... O.xxx }
      checkRef(ThisType.raw(tp.symbol.moduleClass.typeRef))
    case tp @ TermRef(NoPrefix, _) =>
      setting.env.select(tp.symbol)
    case tp @ TermRef(prefix, _) =>
      val res = checkRef(prefix)
      res.value.select(tp.symbol)
    case tp @ ThisType(tref) =>
      val cls = tref.symbol
      if (cls.is(Package)) Res() // Dotty represents package path by ThisType
      else if (setting.env.contains(cls)) Res(value = setting.env(cls))
      else {
        // ThisType used outside of class scope, can happen for objects
        // see tests/pos/t2712-7.scala
        assert(cls.is(Flags.Module) && !setting.ctx.owner.enclosedIn(cls))
        Res()
      }
  })

  def checkClosure(sym: Symbol, tree: Tree)(implicit setting: Setting): Res = {
    if (setting.env.contains(sym)) Res(value = setting.env(sym)) else Res()
  }

  def checkIf(tree: If)(implicit setting: Setting): Res = {
    val If(cond, thenp, elsep) = tree

    val condRes: Res = apply(cond)

    def makeFun(body: Tree) = new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        analyzer.apply(body)
      }

      def widen(implicit setting: Setting) = {
        val res = analyzer.apply(body)
        if (res.hasErrors) ColdValue
        else res.value.widen
      }
    }

    val thenFun = makeFun(thenp)
    val elseFun = makeFun(elsep)

    val res = thenFun.join(elseFun).apply(Nil, Nil)
    res ++= condRes.effects
    res
  }

  def checkValDef(vdef: ValDef)(implicit setting: Setting): Res = {
    val rhsRes =
      if (tpd.isWildcardArg(vdef.rhs)) Res(value = NoValue)
      else apply(vdef.rhs)
    val sym = vdef.symbol

    sym.termRef match {
      case tp @ TermRef(NoPrefix, _) =>
        setting.env.assign(tp.symbol, rhsRes.value)(setting.withPos(vdef.rhs.pos))
      case tp @ TermRef(prefix, _) =>
        val prefixRes = checkRef(prefix)(setting.withPos(vdef.rhs.pos))
        assert(!prefixRes.hasErrors)
        prefixRes.value.assign(sym, rhsRes.value)(setting.withPos(vdef.rhs.pos))
    }

    Res(effects = rhsRes.effects)
  }

  def checkStats(stats: List[Tree])(implicit setting: Setting): Res =
    stats.foldLeft(Res()) { (acc, stat) =>
      indentedDebug(s"acc = ${pad(acc.show(ShowSetting(setting.env.heap, setting.ctx)))}")
      val res1 = apply(stat)
      acc.copy(effects = acc.effects ++ res1.effects)
    }

  def checkBlock(tree: Block)(implicit setting: Setting): Res = {
    val newEnv = setting.env.fresh()
      val setting2 = setting.withEnv(newEnv)
    indexStats(tree.stats)(setting2)

    val res1 = checkStats(tree.stats)(setting2)
    val res2 = apply(tree.expr)(setting2)

    res2.copy(effects = res1.effects ++ res2.effects)
  }

  protected var _methChecking: Set[Symbol] = Set()
  def isChecking(sym: Symbol)   = _methChecking.contains(sym)
  def checking[T](sym: Symbol)(fn: => T) = {
    _methChecking += sym
    val res = fn
    _methChecking -= sym
    res
  }

  def checkAssign(lhs: Tree, rhs: Tree)(implicit setting: Setting): Res = {
    val rhsRes = apply(rhs)
    if (rhsRes.hasErrors) return rhsRes

    lhs match {
      case ident @ Ident(_) =>
        ident.tpe match {
          case tp @ TermRef(NoPrefix, _) =>
            setting.env.assign(tp.symbol, rhsRes.value)(setting.withPos(rhs.pos))
          case tp @ TermRef(prefix, _) =>
            val prefixRes = checkRef(prefix)(setting.withPos(rhs.pos))
            if (prefixRes.hasErrors) prefixRes
            else prefixRes.value.assign(tp.symbol, rhsRes.value)(setting.withPos(rhs.pos))
        }
      case sel @ Select(qual, _) =>
        val prefixRes = apply(qual)
        prefixRes.value.assign(sel.symbol, rhsRes.value)(setting.withPos(rhs.pos))
    }
  }

  /** Check a parent call */
  def checkInit(tp: Type, init: Symbol, argss: List[List[Tree]], obj: ObjectValue)(implicit setting: Setting): Res = {
    if (!init.exists) return Res()

    val cls = init.owner.asClass
    val args = argss.flatten

    // setup constructor params
    var effs = Vector.empty[Effect]
    val argValues = args.map { arg =>
      val res = apply(arg)
      effs = effs ++ res.effects
      res.value
    }

    if (effs.nonEmpty) return Res(effs)

    def toPrefix(tp: Type): Type = tp match {
      case AppliedType(tycon, _) => toPrefix(tycon.dealias)
      case tp: TypeRef => tp.prefix
    }

    val prefix = toPrefix(tp)
    if (prefix == NoPrefix) setting.env.init(init, argValues, args.map(_.pos), obj)
    else {
      val prefixRes = checkRef(prefix)
      if (prefixRes.hasErrors) return prefixRes
      prefixRes.value.init(init, argValues, args.map(_.pos), obj)
    }
  }

  def checkParents(cls: ClassSymbol, parents: List[Tree], obj: ObjectValue)(implicit setting: Setting): Res = {
    if (cls.is(Trait)) return Res()

    def blockInit(stats: List[Tree], parent: Tree, tref: TypeRef, init: Symbol, argss: List[List[Tree]]): Res = {
      val newEnv = setting.env.fresh()
      val setting2 = setting.withEnv(newEnv).withPos(parent.pos)
      indexStats(stats)(setting2)
      val res = checkStats(stats)(setting2)
      res ++ checkInit(parent.tpe, init, argss, obj)(setting2).effects
    }


    // first call super class, see spec 5.1 about "Template Evaluation".
    val res = parents.head match {
      case parent @ NewEx(tref, init, argss) =>
        checkInit(parent.tpe, init, argss, obj)(setting.withPos(parent.pos))
      case Block(stats, parent @ NewEx(tref, init, argss)) =>
        blockInit(stats, parent, tref, init, argss)
      case Apply(Block(stats, parent @ NewEx(tref, init, argss)), args) =>
        blockInit(stats, Apply(parent, args), tref, init, argss :+ args)
    }

    if (res.hasErrors) return res

    val superCls = parents.head.tpe.classSymbol
    val remains = cls.baseClasses.tail.takeWhile(_ `ne` superCls).reverse

    // handle remaning traits
    remains.foldLeft(res) { (acc, traitCls) =>
      val parentOpt = parents.find(_.tpe.classSymbol `eq` traitCls)
      parentOpt match {
        case Some(parent @ NewEx(tref, init, argss)) =>
          checkInit(parent.tpe, init, argss, obj).join(acc)
        case _ =>
          val tp = obj.tp.baseType(traitCls)
          checkInit(tp, traitCls.primaryConstructor, Nil, obj).join(acc)
      }
    }
  }

  def checkNew(tree: Tree, tref: TypeRef, init: Symbol, argss: List[List[Tree]])(implicit setting: Setting): Res = {
    val obj = new ObjectValue(tree.tpe, open = false)
    val res = checkInit(obj.tp, init, argss, obj)
    if (obj.slices.isEmpty) {
      res.copy(value = HotValue)
    }
    else {
      if (res.hasErrors) res.effects = Vector(Instantiate(tree.tpe.classSymbol, res.effects, tree.pos))
      res.copy(value = obj)
    }
  }

  def checkSuper(tree: Tree, supert: Super)(implicit setting: Setting): Res = {
    val SuperType(thistpe, supertpe) = supert.tpe
    val thisRef = checkRef(thistpe)
    thisRef.value.select(tree.symbol, isStaticDispatch = true)
  }

  object NewEx {
    def extract(tp: Type)(implicit ctx: Context): TypeRef = tp.dealias match {
      case tref: TypeRef => tref
      case AppliedType(tref: TypeRef, targs) => tref
    }

    def unapply(tree: tpd.Tree)(implicit ctx: Context): Option[(TypeRef, Symbol, List[List[tpd.Tree]])] = {
      val (fn, targs, vargss) = tpd.decomposeCall(tree)
      if (!fn.symbol.isConstructor || !tree.isInstanceOf[tpd.Apply]) None
      else {
        val Select(New(tpt), _) = fn
        Some((extract(tpt.tpe),  fn.symbol, vargss))
      }
    }
  }

  def apply(tree: Tree)(implicit setting: Setting): Res = trace("checking " + tree.show) {
    doApply(tree)(setting.withPos(tree.pos))
  }

  def doApply(tree: Tree)(implicit setting: Setting): Res = tree match {
    case vdef : ValDef if !vdef.symbol.is(Lazy) && !vdef.rhs.isEmpty =>
      checkValDef(vdef)
    case _: DefTree =>  // ignore, definitions, already indexed
      Res()
    case Closure(_, meth, _) =>
      checkClosure(meth.symbol, tree)
    case tree: Ident if tree.symbol.isTerm =>
      checkRef(tree.tpe)
    case tree: This =>
      checkRef(tree.tpe)
    case tree @ Select(supert: Super, _) =>
      checkSuper(tree, supert)
    case tree: Select if tree.symbol.isTerm =>
      checkSelect(tree)
    case tree: If =>
      checkIf(tree)
    case tree @ NewEx(tref, init, argss) => // must before Apply
      checkNew(tree, tref, init, argss)
    case tree: Apply =>
      val (fn, targs, vargss) = decomposeCall(tree)
      checkApply(tree, fn, vargss)
    case tree @ Assign(lhs, rhs) =>
      checkAssign(lhs, rhs)
    case tree: Block =>
      checkBlock(tree)
    case Typed(expr, tpt) if !tpt.tpe.hasAnnotation(defn.UncheckedAnnot) =>
      apply(expr)
    case _ =>
      Res()
  }
}