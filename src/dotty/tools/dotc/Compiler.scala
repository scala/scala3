package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import typer.{FrontEnd, Typer}

class Compiler {

  def phases = List(new FrontEnd)

  def rootContext(implicit ctx: Context): Context = {
    ctx.usePhases(phases)
    ctx.fresh
      .withPeriod(Period(ctx.runId + 1, FirstPhaseId))
      .withOwner(defn.RootClass)
      .withTyper(new Typer)
  }

  def newRun(implicit ctx: Context): Run =
    new Run(this)(rootContext)
}