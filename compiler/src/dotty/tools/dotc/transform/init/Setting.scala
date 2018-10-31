package dotty.tools.dotc
package transform
package init

import core._
import Symbols._
import Types._
import Contexts.Context
import util.Positions._
import config.Printers.init.{ println => debug }

import collection.mutable

case class Setting(
  env: Env,
  pos: Position,
  ctx: Context,
  analyzer: Analyzer,
  allowDynamic: Boolean = true,
  forceLazy: Boolean = true,
  callParameterless: Boolean = true,
  wideningValues: mutable.Set[Type] = mutable.Set.empty) {
    def strict: Setting = copy(allowDynamic = false, forceLazy = false, callParameterless = false)
    def heap: Heap = env.heap
    def withPos(position: Position) = copy(pos = position)
    def withEnv(ienv: Env) = copy(env = ienv)
    def withCtx(ictx: Context) = copy(ctx = ictx)
    def freshHeap: Setting = {
      val id = env.id
      val heap2 = env.heap.clone
      val env2 = heap2(id)
      copy(env = env2.asEnv)
    }

    def isWidening: Boolean = wideningValues.nonEmpty

    def widenFor(tp: Type)(value: => OpaqueValue): OpaqueValue =
      if (wideningValues.contains(tp)) HotValue
      else {
        wideningValues += tp
        val res = value
        wideningValues -= tp
        res
      }

    def widen(tp: Type): OpaqueValue = widenFor(tp) {
      val res = tp match {
        case tp: TypeRef => // TODO: check class body
          analyzer.checkRef(tp.prefix)(this)
        case _ =>
          analyzer.checkRef(tp)(this)
      }
      analyzer.indentedDebug(res.effects.mkString)
      if (res.hasErrors) WarmValue() else res.value.widen(this)
    }

    def showSetting = ShowSetting(heap, ctx)
  }
