package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import util.Positions._
import config.Printers.init.{ println => debug }


case class Setting(
  env: Env,
  pos: Position,
  ctx: Context,
  analyzer: Analyzer,
  allowDynamic: Boolean = true) {
    def strict: Setting = copy(allowDynamic = false)
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

    def showSetting = ShowSetting(heap, ctx)
  }
