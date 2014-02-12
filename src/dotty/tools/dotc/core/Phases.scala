package dotty.tools.dotc
package core

import Periods._
import Contexts._
import util.DotClass

trait Phases { self: Context =>
  import Phases._

  def phase: Phase = base.phases(period.phaseId)

  def phasesStack: List[Phase] =
    if ((this eq NoContext) || !phase.exists) Nil
    else phase :: outersIterator.dropWhile(_.phase == phase).next.phasesStack

  /** Execute `op` at given phase */
  def atPhase[T](phase: Phase)(op: Context => T): T =
    atPhase(phase.id)(op)

  def atPhaseNotLaterThan[T](limit: Phase)(op: Context => T): T =
    if (!limit.exists || phase <= limit) op(this) else atPhase(limit)(op)

  def atPhaseNotLaterThanTyper[T](op: Context => T): T =
    atPhaseNotLaterThan(base.typerPhase)(op)
}

object Phases {

  trait PhasesBase { this: ContextBase =>

    def allPhases = phases.tail

    object NoPhase extends Phase {
      override def exists = false
      def name = "<no phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
    }

    object SomePhase extends Phase {
      def name = "<some phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
    }

    def phaseNamed(name: String) =
      phases.find(_.name == name).getOrElse(NoPhase)

    /** Use the following phases in the order they are given.
     *  The list should never contain NoPhase.
     */
    def usePhases(phases: List[Phase]) =
      this.phases = (NoPhase :: phases).toArray

    final val typerName = "typer"
    final val refchecksName = "refchecks"
    final val erasureName = "erasure"
    final val flattenName = "flatten"

    lazy val typerPhase = phaseNamed(typerName)
    lazy val refchecksPhase = phaseNamed(refchecksName)
    lazy val erasurePhase = phaseNamed(erasureName)
    lazy val flattenPhase = phaseNamed(flattenName)
  }

  abstract class Phase extends DotClass {

    def name: String

    def run(implicit ctx: Context): Unit

    def runOn(units: List[CompilationUnit])(implicit ctx: Context): Unit =
      for (unit <- units) run(ctx.fresh.withCompilationUnit(unit))

    def description: String = name

    def checkable: Boolean = true

    def exists: Boolean = true

    private[this] var idCache = -1

    /** The sequence position of this phase in the given context where 0
     *  is reserved for NoPhase and the first real phase is at position 1.
     *  Returns -1 if the phase is not installed in the context.
     */
    def id(implicit ctx: Context) = {
      val id = idCache
      val phases = ctx.phases
      if (idCache >= 0 && idCache < phases.length && (phases(idCache) eq this))
        id
      else {
        idCache = phases indexOf this
        idCache
      }
    }

    final def <= (that: Phase)(implicit ctx: Context) =
      exists && id <= that.id

    final def prev(implicit ctx: Context): Phase =
      if (id > FirstPhaseId) ctx.phases(id - 1) else ctx.NoPhase

    final def next(implicit ctx: Context): Phase =
      if (hasNext) ctx.phases(id + 1) else ctx.NoPhase

    final def hasNext(implicit ctx: Context) = id + 1 < ctx.phases.length

    final def iterator(implicit ctx: Context) =
      Iterator.iterate(this)(_.next) takeWhile (_.hasNext)

    final def erasedTypes(implicit ctx: Context): Boolean = ctx.erasurePhase <= this
    final def flatClasses(implicit ctx: Context): Boolean = ctx.flattenPhase <= this
    final def refChecked (implicit ctx: Context): Boolean = ctx.refchecksPhase <= this

    override def toString = name
  }
}