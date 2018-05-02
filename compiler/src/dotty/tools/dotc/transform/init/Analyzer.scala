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

  def trace(msg: => String, env: Env)(body: => Res)(implicit ctx: Context) = {
    indentedDebug(s"==> ${pad(msg)}?")
    indentedDebug("heap = " + env.heap.show)
    indentedDebug(env.show(ShowSetting(env.heap)))
    depth += 1
    val res = body
    depth -= 1
    indentedDebug(s"<== ${pad(msg)} = ${pad(res.show(ShowSetting(env.heap)))}")
    res
  }

  def pad(s: String, padFirst: Boolean = false) = ShowSetting.pad(s, depth, padFirst)

  def indentedDebug(msg: => String) = debug(ShowSetting.pad(msg, depth, padFirst = true))

  def checkApply(tree: tpd.Tree, fun: Tree, argss: List[List[Tree]], env: Env)(implicit ctx: Context): Res = {
    val funSym = fun.symbol
    val funRes = apply(fun, env)

    val args = argss.flatten
    val values = args.map { arg =>
      val res = apply(arg, env)
      funRes ++= res.effects
      res.value
    }

    indentedDebug(s">>> calling $funSym")
    funRes.value(values, args.map(_.pos), tree.pos, env.heap) ++ funRes.effects
  }

  def checkSelect(tree: Select, env: Env)(implicit ctx: Context): Res = {
    val prefixRes = apply(tree.qualifier, env)
    val res = prefixRes.value.select(tree.symbol, env.heap, tree.pos)
    res.effects = prefixRes.effects ++ res.effects
    res
  }

  private def enclosedIn(curSym: Symbol, inSym: Symbol)(implicit ctx: Context): Boolean =
    curSym.exists && ((curSym `eq` inSym) || (enclosedIn(curSym.owner, inSym)))

  def checkRef(tp: Type, env: Env, pos: Position)(implicit ctx: Context): Res = trace("checking " + tp.show, env)(tp match {
    case tp : TermRef if tp.symbol.is(Module) && enclosedIn(ctx.owner, tp.symbol.moduleClass) =>
      // self reference by name: object O { ... O.xxx }
      checkRef(ThisType.raw(tp.symbol.moduleClass.typeRef), env, pos)
    case tp @ TermRef(NoPrefix, _) =>
      env.select(tp.symbol, pos)
    case tp @ TermRef(prefix, _) =>
      val res = checkRef(prefix, env, pos)
      res.value.select(tp.symbol, env.heap, pos)
    case tp @ ThisType(tref) =>
      val cls = tref.symbol
      if (cls.is(Package)) Res() // Dotty represents package path by ThisType
      else if (env.contains(cls)) Res(value = env(cls))
      else {
        // ThisType used outside of class scope, can happen for objects
        // see tests/pos/t2712-7.scala
        assert(cls.is(Flags.Module) && !enclosedIn(ctx.owner, cls))
        Res()
      }
    case tp @ SuperType(thistpe, supertpe) =>
      // TODO : handle `supertpe`
      checkRef(thistpe, env, pos)
  })

  def checkClosure(sym: Symbol, tree: Tree, env: Env)(implicit ctx: Context): Res = {
    if (env.contains(sym)) Res(value = env(sym)) else Res()
  }

  def checkIf(tree: If, env: Env)(implicit ctx: Context): Res = {
    val If(cond, thenp, elsep) = tree

    val condRes: Res = apply(cond, env)

    def makeFun(body: Tree) = new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
        val envCurrent = heap(env.id).asEnv
        analyzer.apply(body, envCurrent)
      }
    }

    val thenFun = makeFun(thenp)
    val elseFun = makeFun(elsep)

    val res = thenFun.join(elseFun).apply(Nil, Nil, NoPosition, env.heap)
    res ++= condRes.effects
    res
  }

  def checkValDef(vdef: ValDef, env: Env)(implicit ctx: Context): Res = {
    val rhsRes = apply(vdef.rhs, env)
    val sym = vdef.symbol

    // take `_` as uninitialized, otherwise it's initialized
    if (!tpd.isWildcardArg(vdef.rhs)) sym.termRef match {
      case tp @ TermRef(NoPrefix, _) =>
        env.assign(tp.symbol, rhsRes.value, vdef.rhs.pos)
      case tp @ TermRef(prefix, _) =>
        val prefixRes = checkRef(prefix, env, vdef.rhs.pos)
        assert(!prefixRes.hasErrors)
        prefixRes.value.assign(sym, rhsRes.value, env.heap, vdef.pos)
    }

    Res(effects = rhsRes.effects)
  }

  def checkStats(stats: List[Tree], env: Env)(implicit ctx: Context): Res =
    stats.foldLeft(Res()) { (acc, stat) =>
      indentedDebug(s"acc = ${pad(acc.show(ShowSetting(env.heap)))}")
      val res1 = apply(stat, env)
      acc.copy(effects = acc.effects ++ res1.effects)
    }

  def checkBlock(tree: Block, env: Env)(implicit ctx: Context): Res = {
    val newEnv = env.fresh()
    indexStats(tree.stats, newEnv)

    val res1 = checkStats(tree.stats, newEnv)
    val res2 = apply(tree.expr, newEnv)

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

  def checkAssign(lhs: Tree, rhs: Tree, env: Env)(implicit ctx: Context): Res = {
    val rhsRes = apply(rhs, env)
    if (rhsRes.hasErrors) return rhsRes

    lhs match {
      case ident @ Ident(_) =>
        ident.tpe match {
          case tp @ TermRef(NoPrefix, _) =>
            env.assign(tp.symbol, rhsRes.value, rhs.pos)
          case tp @ TermRef(prefix, _) =>
            val prefixRes = checkRef(prefix, env, rhs.pos)
            if (prefixRes.hasErrors) prefixRes
            else prefixRes.value.assign(tp.symbol, rhsRes.value, env.heap, rhs.pos)
        }
      case sel @ Select(qual, _) =>
        val prefixRes = apply(qual, env)
        prefixRes.value.assign(sel.symbol, rhsRes.value, env.heap, rhs.pos)
    }
  }

  /** Check a parent call */
  def checkInit(tp: Type, init: Symbol, argss: List[List[Tree]], env: Env, obj: ObjectValue, pos: Position)(implicit ctx: Context): Res = {
    if (!init.exists) return Res()

    val cls = init.owner.asClass
    val args = argss.flatten

    // setup constructor params
    var effs = Vector.empty[Effect]
    val argValues = args.map { arg =>
      val res = apply(arg, env)
      effs = effs ++ res.effects
      res.value
    }

    if (effs.nonEmpty) return Res(effs)

    def toPrefix(tp: Type): Type = tp match {
      case AppliedType(tycon, _) => toPrefix(tycon.dealias)
      case tp: TypeRef => tp.prefix
    }

    val prefix = toPrefix(tp)
    if (prefix == NoPrefix) env.init(init, argValues, args.map(_.pos), pos, obj, this)
    else {
      val prefixRes = checkRef(prefix, env, pos)
      if (prefixRes.hasErrors) return prefixRes
      prefixRes.value.init(init, argValues, args.map(_.pos), pos, obj, env.heap, this)
    }
  }

  def checkParents(cls: ClassSymbol, parents: List[Tree], env: Env, obj: ObjectValue)(implicit ctx: Context): Res = {
    if (cls.is(Trait)) return Res()

    def blockInit(stats: List[Tree], parent: Tree, tref: TypeRef, init: Symbol, argss: List[List[Tree]]): Res = {
      val newEnv = env.fresh()
      indexStats(stats, newEnv)
      val res = checkStats(stats, newEnv)
      res ++ checkInit(parent.tpe, init, argss, newEnv, obj, parent.pos).effects
    }


    // first call super class, see spec 5.1 about "Template Evaluation".
    val res = parents.head match {
      case parent @ NewEx(tref, init, argss) =>
        checkInit(parent.tpe, init, argss, env, obj, parent.pos)
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
          checkInit(parent.tpe, init, argss, env, obj, parent.pos).join(acc)
        case _ =>
          val tp = obj.tp.baseType(traitCls)
          checkInit(tp, traitCls.primaryConstructor, Nil, env, obj, cls.pos).join(acc)
      }
    }
  }

  def checkNew(tree: Tree, tref: TypeRef, init: Symbol, argss: List[List[Tree]], env: Env)(implicit ctx: Context): Res = {
    val obj = new ObjectValue(tree.tpe, open = false)
    val res = checkInit(obj.tp, init, argss, env, obj, tree.pos)
    if (obj.slices.isEmpty) {
      res.copy(value = FullValue)
    }
    else {
      if (res.hasErrors) res.effects = Vector(Instantiate(tree.tpe.classSymbol, res.effects, tree.pos))
      res.copy(value = obj)
    }
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

  def apply(tree: Tree, env: Env)(implicit ctx: Context): Res = trace("checking " + tree.show, env)(tree match {
    case vdef : ValDef if !vdef.symbol.is(Lazy) && !vdef.rhs.isEmpty =>
      checkValDef(vdef, env)
    case _: DefTree =>  // ignore, definitions, already indexed
      Res()
    case Closure(_, meth, _) =>
      checkClosure(meth.symbol, tree, env)
    case tree: Ident if tree.symbol.isTerm =>
      checkRef(tree.tpe, env, tree.pos)
    case tree: This =>
      checkRef(tree.tpe, env, tree.pos)
    case tree: Super =>
      checkRef(tree.tpe, env, tree.pos)
    case tree: Select if tree.symbol.isTerm =>
      checkSelect(tree, env)
    case tree: If =>
      checkIf(tree, env)
    case tree @ NewEx(tref, init, argss) => // must before Apply
      checkNew(tree, tref, init, argss, env)
    case tree: Apply =>
      val (fn, targs, vargss) = decomposeCall(tree)
      checkApply(tree, fn, vargss, env)
    case tree @ Assign(lhs, rhs) =>
      checkAssign(lhs, rhs, env)
    case tree: Block =>
      checkBlock(tree, env)
    case Typed(expr, tpt) if !tpt.tpe.hasAnnotation(defn.UncheckedAnnot) =>
      apply(expr, env)
    case _ =>
      Res()
  })
}