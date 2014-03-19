package dotty.tools.dotc
package core

import Periods._
import Contexts._
import util.DotClass
import DenotTransformers._
import Denotations._
import config.Printers._

trait Phases { self: Context =>
  import Phases._

  def phase: Phase = base.phases(period.phaseId)

  def phasesStack: List[Phase] =
    if ((this eq NoContext) || !phase.exists) Nil
    else phase :: outersIterator.dropWhile(_.phase == phase).next.phasesStack

  /** Execute `op` at given phase */
  def atPhase[T](phase: Phase)(op: Context => T): T =
    atPhase(phase.id)(op)

  def atNextPhase[T](op: Context => T): T = atPhase(phase.next)(op)

  def atPhaseNotLaterThan[T](limit: Phase)(op: Context => T): T =
    if (!limit.exists || phase <= limit) op(this) else atPhase(limit)(op)

  def atPhaseNotLaterThanTyper[T](op: Context => T): T =
    atPhaseNotLaterThan(base.typerPhase)(op)
}

object Phases {

  trait PhasesBase { this: ContextBase =>

    def allPhases = phases.tail // drop NoPhase at beginning

    object NoPhase extends Phase {
      override def exists = false
      def name = "<no phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = unsupported("transform")
    }

    object SomePhase extends Phase {
      def name = "<some phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
    }

    /** A sentinel transformer object */
    class TerminalPhase extends DenotTransformer {
      def name = "terminal"
      def run(implicit ctx: Context): Unit = unsupported("run")
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
        unsupported("transform")
      override def lastPhaseId(implicit ctx: Context) = id
    }

    def phaseNamed(name: String) =
      phases.find(_.name == name).getOrElse(NoPhase)

    /** Use the following phases in the order they are given.
     *  The list should never contain NoPhase.
     */
    def usePhases(phases: List[Phase]) = {
      this.phases = (NoPhase :: phases ::: new TerminalPhase :: Nil).toArray
      this.nextTransformerId = new Array[Int](this.phases.length)
      var i = 0
      while (i < this.phases.length) {
        this.phases(i)._id = i
        i += 1
      }
      var lastTransformerId = i
      while (i > 0) {
        i -= 1
        if (this.phases(i).isInstanceOf[DenotTransformer]) lastTransformerId = i
        nextTransformerId(i) = lastTransformerId
      }
      config.println(s"Phases = ${this.phases.deep}")
      config.println(s"nextTransformId = ${nextTransformerId.deep}")
    }

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
      for (unit <- units) run(ctx.fresh.withPhase(this).withCompilationUnit(unit))

    def description: String = name

    def checkable: Boolean = true

    def exists: Boolean = true

    private[Phases] var _id = -1

    /** The sequence position of this phase in the given context where 0
     *  is reserved for NoPhase and the first real phase is at position 1.
     *  -1 if the phase is not installed in the context.
     */
    def id = _id

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