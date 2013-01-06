package dotty.tools.dotc
package core

import Decorators._
import Periods._
import Names._
import Phases._
import Types._
import SubTypers._

object Contexts {

  val NoContext: Context = null

  abstract class Context extends Periods {
    val underlying: Context
    val root: RootContext
    val period: Period
    def constraints: Constraints
    def subTyper: SubTyper
    def names: NameTable
    def phase: Phase = ???
    def stableInterval: Interval = ???
    def erasedTypes: Boolean = ???
  }

  abstract class SubContext(val underlying: Context) extends Context {
    val root: RootContext = underlying.root
    val period: Period = underlying.period
    val constraints = underlying.constraints
    def names: NameTable = root.names
    lazy val subTyper =
      if (constraints eq underlying.constraints) underlying.subTyper
      else new SubTyper(this)
  }

  class RootContext extends Context
                       with Symbols
                       with Denotations
                       with DenotationTransformers
                       with Types {

    val underlying: Context = throw new UnsupportedOperationException("RootContext.underlying")
    def subTyper: SubTyper = ???

    val root: RootContext = this
    val period = periodOf(NoRunId, NoPhaseId)
    val names: NameTable = new NameTable
    val variance = 1

    var lastPhaseId: Int = NoPhaseId
    lazy val definitions = new Definitions()(this)

    val constraints: Constraints = Map()
  }

  private final val initialUniquesCapacity = 4096
}