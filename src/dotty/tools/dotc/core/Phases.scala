package dotty.tools.dotc
package core

import Periods._
import Contexts._
import util.DotClass
import DenotTransformers._
import Denotations._
import config.Printers._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransformer, MiniPhase, TreeTransform}
import dotty.tools.dotc.transform.TreeTransforms
import Periods._
import typer.{FrontEnd, RefChecks}
import ast.tpd
import dotty.tools.dotc.transform.{Erasure, Flatten}

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
      def phaseName = "<no phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = unsupported("transform")
    }

    object SomePhase extends Phase {
      def phaseName = "<some phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
    }

    /** A sentinel transformer object */
    class TerminalPhase extends DenotTransformer {
      def phaseName = "terminal"
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
      var prevPhases: Set[Class[_ <: Phase]] = Set.empty
      var i = 0
      while (i < phasess.length) {
        if (phasess(i).length > 1) {
          val phasesInBlock: Set[String] = phasess(i).map(_.phaseName).toSet
          for(phase<-phasess(i)) {
            phase match {
              case p: MiniPhase =>

                val unmetRequirements = p.runsAfterGroupsOf &~ prevPhases
                assert(unmetRequirements.isEmpty,
                  s"${phase.phaseName} requires ${unmetRequirements.mkString(", ")} to be in different TreeTransformer")

              case _ =>
                assert(false, s"Only tree transforms can be squashed, ${phase.phaseName} can not be squashed")
            }
          }
          val transforms = phasess(i).asInstanceOf[List[MiniPhase]].map(_.treeTransform)
          val block = new TreeTransformer {
            override def phaseName: String = transformations.map(_.phase.phaseName).mkString("TreeTransform:{", ", ", "}")
            override def transformations: Array[TreeTransform] = transforms.toArray
          }
          squashedPhases += block
          prevPhases ++= phasess(i).map(_.getClazz)
          block.init(this, phasess(i).head.id, phasess(i).last.id)
        } else {
          squashedPhases += phasess(i).head
          prevPhases += phasess(i).head.getClazz
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
      var phasesAfter:Set[Class[_ <: Phase]] = Set.empty
      nextDenotTransformerId = new Array[Int](phases.length)
      denotTransformers = new Array[DenotTransformer](phases.length)
      var i = 0
      while (i < phases.length) {
        phases(i).init(this, i)
        val unmetPreceedeRequirements = phases(i).runsAfter -- phasesAfter
        assert(unmetPreceedeRequirements.isEmpty,
          s"phase ${phases(i)} has unmet requirement: ${unmetPreceedeRequirements.mkString(", ")} should precede this phase")
        phasesAfter += phases(i).getClazz
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

    def phaseOfClass(pclass: Class[_]) = phases.find(pclass.isInstance).getOrElse(NoPhase)

    /** A cache to compute the phase with given name, which
     *  stores the phase as soon as phaseNamed returns something
     *  different from NoPhase.
     */
    private class PhaseCache(pclass: Class[_ <: Phase]) {
      private var myPhase: Phase = NoPhase
      def phase = {
        if (myPhase eq NoPhase) myPhase = phaseOfClass(pclass)
        myPhase
      }
    }

    private val typerCache = new PhaseCache(classOf[FrontEnd])
    private val refChecksCache = new PhaseCache(classOf[RefChecks])
    private val erasureCache = new PhaseCache(classOf[Erasure])
    private val flattenCache = new PhaseCache(classOf[Flatten])

    def typerPhase = typerCache.phase
    def refchecksPhase = refChecksCache.phase
    def erasurePhase = erasureCache.phase
    def flattenPhase = flattenCache.phase

    def isAfterTyper(phase: Phase): Boolean = phase.id > typerPhase.id
  }

  trait Phase extends DotClass {

    def phaseName: String

    /** List of names of phases that should precede this phase */
    def runsAfter: Set[Class[_ <: Phase]] = Set.empty

    def run(implicit ctx: Context): Unit

    def runOn(units: List[CompilationUnit])(implicit ctx: Context): Unit =
      for (unit <- units) run(ctx.fresh.setPhase(this).setCompilationUnit(unit))

    def description: String = phaseName

    /** Output should be checkable by TreeChecker */
    def isCheckable: Boolean = true

    /** Check what the phase achieves, to be called at any point after it is finished.
     */
    def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = ()

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
      myErasedTypes = prev.getClass == classOf[Erasure]   || prev.erasedTypes
      myFlatClasses = prev.getClass == classOf[Flatten]   || prev.flatClasses
      myRefChecked  = prev.getClass == classOf[RefChecks] || prev.refChecked
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

    override def toString = phaseName
  }

  /** Dotty deviation: getClass yields Class[_], instead of [Class <: <type of receiver>].
   *  We can get back the old behavior using this decorator. We should also use the same
   *  trick for standard getClass.
   */
  private implicit class getClassDeco[T](val x: T) extends AnyVal {
    def getClazz: Class[_ <: T] = x.getClass.asInstanceOf[Class[_ <: T]]
  }
}