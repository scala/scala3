package dotty.tools.dotc
package transform
package init

import core._
import Symbols._
import Types._
import Contexts.Context
import util.SimpleIdentitySet
import util.Positions._
import config.Printers.init.{ println => debug }

import collection.mutable

case class Setting(
  env: Env,
  pos: Position,
  ctx: Context,
  analyzer: Analyzer,
  isWidening: Boolean = false,
  propagateDeps: Boolean = false,
  var wideningValues: SimpleIdentitySet[Value] = SimpleIdentitySet.empty) {
    def widening: Setting = copy(isWidening = true)
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

    def widenFor(v: Value)(value: => OpaqueValue): OpaqueValue =
      if (wideningValues.contains(v)) HotValue
      else {
        analyzer.indentedDebug(s"widening ${v.show(this.showSetting)} = ?")
        wideningValues = wideningValues + v
        val res = value
        wideningValues = wideningValues - v
        analyzer.indentedDebug(s"widening ${v.show(this.showSetting)} = " + res.show(this.showSetting))
        res
      }

    def widen(tp: Type): OpaqueValue = {
      val res = analyzer.checkRef(tp)(this)
      if (res.hasErrors) WarmValue() else res.value.widen(this)
    }

    def showSetting = ShowSetting(heap, ctx)

    override def toString = s"Setting(isWidening = $isWidening, propagateDeps = $propagateDeps)"
  }
