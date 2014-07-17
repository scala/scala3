package dotty.tools.dotc
package core

import Periods._
import Contexts._
import util.DotClass
import DenotTransformers._
import Denotations._
import config.Printers._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransformer, TreeTransform}
import dotty.tools.dotc.transform.TreeTransforms
import TreeTransforms.Separator
import Periods._

trait Phases {
  self: Context =>

  import Phases._

  def phase: Phase = base.phases(period.firstPhaseId)

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

  def isAfterTyper: Boolean = base.isAfterTyper(phase)
}

object Phases {

  trait PhasesBase {
    this: ContextBase =>

    // drop NoPhase at beginning
    def allPhases = squashedPhases.tail

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

    /** Squash TreeTransform's beloning to same sublist to a single TreeTransformer
      * Each TreeTransform gets own period,
      * whereas a combined TreeTransformer gets period equal to union of periods of it's TreeTransforms
      */
    private def squashPhases(phasess: List[List[Phase]]): Array[Phase] = {
      val squashedPhases = ListBuffer[Phase]()
      var prevPhases: Set[String] = Set.empty
      var i = 0
      while (i < phasess.length) {
        if (phasess(i).length > 1) {
          val phasesInBlock: Set[String] = phasess(i).map(_.name).toSet
          for(phase<-phasess(i)) {
            phase match {
              case p: TreeTransform =>

                val unmetRequirements = p.runsAfterGroupsOf &~ prevPhases
                assert(unmetRequirements.isEmpty,
                  s"${phase.name} requires ${unmetRequirements.mkString(", ")} to be in different TreeTransformer")

              case _ =>
                assert(false, s"Only tree transforms can be squashed, ${phase.name} can not be squashed")
            }
          }
          val transforms = phasess(i).asInstanceOf[List[TreeTransform]]
          val block = new TreeTransformer {
            override def name: String = transformations.map(_.name).mkString("TreeTransform:{", ", ", "}")
            override def transformations: Array[TreeTransform] = transforms.toArray
          }
          squashedPhases += block
          prevPhases ++= phasess(i).map(_.name)
          block.init(this, phasess(i).head.id, phasess(i).last.id)
        } else {
          squashedPhases += phasess(i).head
          prevPhases += phasess(i).head.name
        }
        i += 1
      }
      (NoPhase :: squashedPhases.toList ::: new TerminalPhase :: Nil).toArray
    }

    /** Use the following phases in the order they are given.
     *  The list should never contain NoPhase.
     *  if squashing is enabled, phases in same subgroup will be squashed to single phase.
     */
    def usePhases(phasess: List[List[Phase]], squash: Boolean = true) = {
      phases = (NoPhase :: phasess.flatten ::: new TerminalPhase :: Nil).toArray
      var phasesAfter:Set[String] = Set.empty
      nextDenotTransformerId = new Array[Int](phases.length)
      denotTransformers = new Array[DenotTransformer](phases.length)
      var i = 0
      while (i < phases.length) {
        phases(i).init(this, i)
        val unmetPreceedeRequirements = phases(i).runsAfter -- phasesAfter
        assert(unmetPreceedeRequirements.isEmpty,
          s"phase ${phases(i)} has unmet requirement: ${unmetPreceedeRequirements.mkString(", ")} should precede this phase")
        phasesAfter += phases(i).name
        i += 1
      }
      var lastTransformerId = i
      while (i > 0) {
        i -= 1
        phases(i) match {
          case transformer: DenotTransformer =>
            lastTransformerId = i
            denotTransformers(i) = transformer
          case _ =>
        }
        nextDenotTransformerId(i) = lastTransformerId
      }

      if (squash) {
        this.squashedPhases = squashPhases(phasess)
      } else {
        this.squashedPhases = this.phases
      }

      config.println(s"Phases = ${phases.deep}")
      config.println(s"squashedPhases = ${squashedPhases.deep}")
      config.println(s"nextDenotTransformerId = ${nextDenotTransformerId.deep}")
    }

    def phaseNamed(name: String) = phases.find(_.name == name).getOrElse(NoPhase)

    /** A cache to compute the phase with given name, which
     *  stores the phase as soon as phaseNamed returns something
     *  different from NoPhase.
     */
    private class PhaseCache(name: String) {
      private var myPhase: Phase = NoPhase
      def phase = {
        if (myPhase eq NoPhase) myPhase = phaseNamed(name)
        myPhase
      }
    }

    private val typerCache = new PhaseCache(typerName)
    private val refChecksCache = new PhaseCache(refChecksName)
    private val erasureCache = new PhaseCache(erasureName)
    private val flattenCache = new PhaseCache(flattenName)

    def typerPhase = typerCache.phase
    def refchecksPhase = refChecksCache.phase
    def erasurePhase = erasureCache.phase
    def flattenPhase = flattenCache.phase

    def isAfterTyper(phase: Phase): Boolean = phase.id > typerPhase.id
  }

  final val typerName = "frontend"
  final val refChecksName = "refchecks"
  final val erasureName = "erasure"
  final val flattenName = "flatten"

  abstract class Phase extends DotClass {

    def name: String

    /** List of names of phases that should precede this phase */
    def runsAfter: Set[String] = Set.empty

    def run(implicit ctx: Context): Unit

    def runOn(units: List[CompilationUnit])(implicit ctx: Context): Unit =
      for (unit <- units) run(ctx.fresh.setPhase(this).setCompilationUnit(unit))

    def description: String = name

    def checkable: Boolean = true

    def exists: Boolean = true

    private var myPeriod: Period = Periods.InvalidPeriod
    private var myBase: ContextBase = null
    private var myErasedTypes = false
    private var myFlatClasses = false
    private var myRefChecked = false

    /** The sequence position of this phase in the given context where 0
     * is reserved for NoPhase and the first real phase is at position 1.
     * -1 if the phase is not installed in the context.
     */
    def id = myPeriod.firstPhaseId

    def period = myPeriod
    def start = myPeriod.firstPhaseId
    def end = myPeriod.lastPhaseId

    final def erasedTypes = myErasedTypes
    final def flatClasses = myFlatClasses
    final def refChecked = myRefChecked

    protected[Phases] def init(base: ContextBase, start: Int, end:Int): Unit = {
      if (start >= FirstPhaseId)
        assert(myPeriod == Periods.InvalidPeriod, s"phase $this has already been used once; cannot be reused")
      myBase = base
      myPeriod = Period(start, end)
      myErasedTypes = prev.name == erasureName   || prev.erasedTypes
      myFlatClasses = prev.name == flattenName   || prev.flatClasses
      myRefChecked  = prev.name == refChecksName || prev.refChecked
    }

    protected[Phases] def init(base: ContextBase, id: Int): Unit = init(base, id, id)

    final def <=(that: Phase)(implicit ctx: Context) =
      exists && id <= that.id

    final def prev: Phase =
      if (id > FirstPhaseId) myBase.phases(start - 1) else myBase.NoPhase

    final def next: Phase =
      if (hasNext) myBase.phases(end + 1) else myBase.NoPhase

    final def hasNext = start >= FirstPhaseId && end + 1 < myBase.phases.length

    final def iterator =
      Iterator.iterate(this)(_.next) takeWhile (_.hasNext)

    override def toString = name
  }
}