package dotty.tools.dotc
package core

import Periods._, Contexts._

trait Phases { self: Context =>
  import Phases._

  def phase: Phase = base.phases(period.phaseId)

  def phasesStack: List[Phase] =
    if ((this eq NoContext) || !phase.exists) Nil
    else phase :: outersIterator.dropWhile(_.phase == phase).next.phasesStack

  /** Execute `op` at given phase id */
  def atPhase[T](phase: Phase)(op: Context => T): T =
    atPhase(phase.id)(op)

  def atPhaseNotLaterThan[T](limit: Phase)(op: Context => T): T =
    if (!limit.exists || phase <= limit) op(this) else atPhase(limit)(op)

  def atPhaseNotLaterThanTyper[T](op: Context => T): T =
    atPhaseNotLaterThan(base.typerPhase)(op)
}

object Phases {

  trait PhasesBase { this: ContextBase =>

    lazy val allPhases = phases.slice(FirstPhaseId, nphases)

    object NoPhase extends Phase(initialCtx) {
      override def exists = false
      def name = "<no phase>"
      def run() { throw new Error("NoPhase.run") }
    }

    object SomePhase extends Phase(initialCtx) {
      def name = "<some phase>"
      def run() { throw new Error("SomePhase.run") }
    }

    def phaseNamed(name: String) =
      allPhases.find(_.name == name).getOrElse(NoPhase)

    final val typerName = "typer"
    final val refchecksName = "refchecks"
    final val erasureName = "erasure"
    final val flattenName = "flatten"

    lazy val typerPhase = phaseNamed(typerName)
    lazy val refchecksPhase = phaseNamed(refchecksName)
    lazy val erasurePhase = phaseNamed(erasureName)
    lazy val flattenPhase = phaseNamed(flattenName)
  }

  abstract class Phase(initctx: Context) {

    val id: Int = initctx.nphases
    initctx.phases(id) = this  // TODO: Do explicit phase install instead?
    initctx.nphases += 1

    def name: String

    def run(): Unit

    def description: String = name

    def checkable: Boolean = true

    def exists: Boolean = true

    final def <= (that: Phase) =
      exists && id <= that.id

    final def prev(implicit ctx: Context): Phase =
      if (id > FirstPhaseId) ctx.phases(id - 1) else initctx.NoPhase

    final def next(implicit ctx: Context): Phase =
      if (hasNext) ctx.phases(id + 1) else initctx.NoPhase

    final def hasNext(implicit ctx: Context) = id + 1 < ctx.nphases

    final def iterator(implicit ctx: Context) =
      Iterator.iterate(this)(_.next) takeWhile (_.hasNext)

    final def erasedTypes(implicit ctx: Context): Boolean = ctx.erasurePhase <= this
    final def flatClasses(implicit ctx: Context): Boolean = ctx.flattenPhase <= this
    final def refChecked (implicit ctx: Context): Boolean = ctx.refchecksPhase <= this

    override def toString = name
  }
}