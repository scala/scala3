package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context

import Potentials._, Effects._

object Summary {
  type Summary = (Potentials, Effects)
  val empty: Summary = (Potentials.empty, Effects.empty)

  def show(summary: Summary)(implicit ctx: Context): String = {
    val pots = Potentials.show(summary._1)
    val effs = Effects.show(summary._2)
    s"([$pots], [$effs])"
  }
}
