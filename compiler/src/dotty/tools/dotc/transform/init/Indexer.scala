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

  def methodValue(ddef: DefDef, env: Env)(implicit ctx: Context): FunctionValue =
    new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res =
        if (isChecking(ddef.symbol)) {
          // TODO: check if fixed point has reached. But the domain is infinite, thus non-terminating.
          debug(s"recursive call of ${ddef.symbol} found")
          Res()
        }
        else {
          val env2 = env.fresh(heap)

          ddef.vparamss.flatten.zipWithIndex.foreach { case (param: ValDef, index) =>
            env2.add(param.symbol, value = values(index))
          }
          val res = checking(ddef.symbol) { self.apply(ddef.rhs, env2)(ctx.withOwner(ddef.symbol)) }
          if (res.hasErrors) res.effects = Vector(Call(ddef.symbol, res.effects, pos))
          res
        }
    }

  def lazyValue(vdef: ValDef, env: Env)(implicit ctx: Context): LazyValue =
    new LazyValue {
      def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res =
        if (isChecking(vdef.symbol)) {
          // TODO: check if fixed point has reached. But the domain is infinite, thus non-terminating.
          debug(s"recursive call of ${vdef.symbol} found")
          Res()
        }
        else {
          val env2 = heap(env.id).asEnv
          val res = checking(vdef.symbol) { self.apply(vdef.rhs, env2)(ctx.withOwner(vdef.symbol)) }
          if (res.hasErrors) res.effects = Vector(Force(vdef.symbol, res.effects, pos))
          res
        }
    }

  /** Index local definitions */
  def indexStats(stats: List[Tree], env: Env)(implicit ctx: Context): Unit = stats.foreach {
    case ddef: DefDef if !ddef.symbol.isConstructor =>  // TODO: handle secondary constructor
      env.add(ddef.symbol, methodValue(ddef, env))
    case vdef: ValDef if vdef.symbol.is(Lazy)  =>
      env.add(vdef.symbol, lazyValue(vdef, env))
    case vdef: ValDef =>
      env.add(vdef.symbol, NoValue)
    case tdef: TypeDef if tdef.isClassDef  =>
      // class has to be handled differently because of inheritance
      env.addClassDef(tdef.symbol.asClass, tdef.rhs.asInstanceOf[Template])
    case _ =>
  }

  /** Index member definitions
   *
   *  trick: use `slice` for name resolution, but `env` for method execution
   */
  def indexMembers(stats: List[Tree], slice: SliceRep)(implicit ctx: Context): Unit = stats.foreach {
    case ddef: DefDef =>
      slice.add(ddef.symbol, methodValue(ddef, slice.innerEnv))
    case vdef: ValDef if vdef.symbol.is(Lazy)  =>
      slice.add(vdef.symbol, lazyValue(vdef, slice.innerEnv))
    case vdef: ValDef =>
      val value = if (vdef.symbol.isInit) FullValue else NoValue
      slice.add(vdef.symbol, value)
    case tdef: TypeDef if tdef.isClassDef  =>
      // class has to be handled differently because of inheritance
      slice.add(tdef.symbol.asClass, tdef.rhs.asInstanceOf[Template])
    case _ =>
  }

  def init(constr: Symbol, tmpl: Template, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, env: Env)(implicit ctx: Context): Res = {
    val cls = constr.owner.asClass

    if (isChecking(cls)) {
      debug(s"recursive creation of $cls found")
      Res()
    }
    else checking(cls) {
      val slice = env.newSlice(cls)
      obj.add(cls, new SliceValue(slice.id))

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
      val res = checkParents(cls, tmpl.parents, slice.innerEnv, obj)(ctx.withOwner(cls.owner))
      if (res.hasErrors) return res

      // check current class body
      res ++= checkStats(tmpl.body, slice.innerEnv)(ctx.withOwner(cls)).effects
      res
    }
  }
}
