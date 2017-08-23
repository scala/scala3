package dotty.tools.dotc
package core

import Periods._
import Contexts._
import dotty.tools.backend.jvm.{LabelDefs, GenBCode}
import dotty.tools.dotc.core.Symbols.ClassSymbol
import util.DotClass
import DenotTransformers._
import Denotations._
import Decorators._
import config.Printers.config
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransformer, MiniPhase, TreeTransform}
import dotty.tools.dotc.transform._
import Periods._
import typer.{FrontEnd, RefChecks}
import ast.tpd

trait Phases {
  self: Context =>

  import Phases._

  def phase: Phase = base.phases(period.firstPhaseId)

  def phasesStack: List[Phase] =
    if ((this eq NoContext) || !phase.exists) Nil
    else {
      val rest = outersIterator.dropWhile(_.phase == phase)
      phase :: (if (rest.hasNext) rest.next().phasesStack else Nil)
    }

  /** Execute `op` at given phase */
  def atPhase[T](phase: Phase)(op: Context => T): T =
    atPhase(phase.id)(op)

  def atNextPhase[T](op: Context => T): T = atPhase(phase.next)(op)

  def atPhaseNotLaterThan[T](limit: Phase)(op: Context => T): T =
    if (!limit.exists || phase <= limit) op(this) else atPhase(limit)(op)

  def isAfterTyper: Boolean = base.isAfterTyper(phase)
}

object Phases {

  trait PhasesBase {
    this: ContextBase =>

    // drop NoPhase at beginning
    def allPhases = (if (squashedPhases.nonEmpty) squashedPhases else phases).tail

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

    def phasePlan = this.phasesPlan
    def setPhasePlan(phasess: List[List[Phase]]) = this.phasesPlan = phasess

    /** Squash TreeTransform's beloning to same sublist to a single TreeTransformer
      * Each TreeTransform gets own period,
      * whereas a combined TreeTransformer gets period equal to union of periods of it's TreeTransforms
      */
    def squashPhases(phasess: List[List[Phase]],
                             phasesToSkip: List[String], stopBeforePhases: List[String], stopAfterPhases: List[String], YCheckAfter: List[String]): List[Phase] = {
      val squashedPhases = ListBuffer[Phase]()
      var prevPhases: Set[Class[_ <: Phase]] = Set.empty
      val YCheckAll = YCheckAfter.contains("all")

      var stop = false
      val filteredPhases = phasess.map(_.filter { p =>
        val pstop = stop
        stop = stop | stopBeforePhases.contains(p.phaseName) | stopAfterPhases.contains(p.phaseName)
        !(pstop || stopBeforePhases.contains(p.phaseName) || phasesToSkip.contains(p.phaseName))
      })

      var i = 0

      while (i < filteredPhases.length) {
        if (filteredPhases(i).nonEmpty) { //could be empty due to filtering
          val filteredPhaseBlock = filteredPhases(i)
          val phaseToAdd =
            if (filteredPhaseBlock.length > 1) {
              val phasesInBlock: Set[String] = filteredPhaseBlock.map(_.phaseName).toSet
              for (phase <- filteredPhaseBlock) {
                phase match {
                  case p: MiniPhase =>
                    val unmetRequirements = p.runsAfterGroupsOf &~ prevPhases
                    assert(unmetRequirements.isEmpty,
                      s"${phase.phaseName} requires ${unmetRequirements.mkString(", ")} to be in different TreeTransformer")

                  case _ =>
                    assert(false, s"Only tree transforms can be squashed, ${phase.phaseName} can not be squashed")
                }
              }
              val block = new TreeTransformer {
                override def phaseName: String = miniPhases.map(_.phaseName).mkString("TreeTransform:{", ", ", "}")
                override def miniPhases: Array[MiniPhase] = filteredPhaseBlock.asInstanceOf[List[MiniPhase]].toArray
              }
              prevPhases ++= filteredPhaseBlock.map(_.getClazz)
              block
            } else { // block of a single phase, no squashing
              val phase = filteredPhaseBlock.head
              prevPhases += phase.getClazz
              phase
            }
          squashedPhases += phaseToAdd
          val shouldAddYCheck = YCheckAfter.containsPhase(phaseToAdd) || YCheckAll
          if (shouldAddYCheck) {
            val checker = new TreeChecker
            squashedPhases += checker
          }
        }

        i += 1
      }
      squashedPhases.toList
    }

    /** Use the following phases in the order they are given.
     *  The list should never contain NoPhase.
     *  if squashing is enabled, phases in same subgroup will be squashed to single phase.
     */
    def usePhases(phasess: List[Phase], squash: Boolean = true) = {

      val flatPhases = collection.mutable.ListBuffer[Phase]()

      phasess.foreach(p => p match {
        case t: TreeTransformer => flatPhases ++= t.miniPhases
        case _ => flatPhases += p
      })

      phases = (NoPhase :: flatPhases.toList ::: new TerminalPhase :: Nil).toArray
      var phasesAfter:Set[Class[_ <: Phase]] = Set.empty
      nextDenotTransformerId = new Array[Int](phases.length)
      denotTransformers = new Array[DenotTransformer](phases.length)

      var phaseId = 0
      def nextPhaseId = {
        phaseId += 1
        phaseId // starting from 1 as NoPhase is 0
      }

      def checkRequirements(p: Phase) = {
        val unmetPrecedeRequirements = p.runsAfter -- phasesAfter
        assert(unmetPrecedeRequirements.isEmpty,
          s"phase ${p} has unmet requirement: ${unmetPrecedeRequirements.mkString(", ")} should precede this phase")
        phasesAfter += p.getClazz

      }
      var i = 0

      while (i < phasess.length) {
        val phase = phasess(i)
        phase match {
          case t: TreeTransformer =>
            val miniPhases = t.miniPhases
            miniPhases.foreach{ phase =>
              checkRequirements(phase)
              phase.init(this, nextPhaseId)}
            t.init(this, miniPhases.head.id, miniPhases.last.id)
          case _ =>
            phase.init(this, nextPhaseId)
            checkRequirements(phase)
        }

        i += 1
      }

      phases.last.init(this, nextPhaseId) // init terminal phase

      i = phases.length
      var lastTransformerId = i
      while (i > 0) {
        i -= 1
        val phase = phases(i)
        phase match {
          case transformer: DenotTransformer =>
            lastTransformerId = i
            denotTransformers(i) = transformer
          case _ =>
        }
        nextDenotTransformerId(i) = lastTransformerId
      }

      if (squash) {
        this.squashedPhases = (NoPhase :: phasess).toArray
      } else {
        this.squashedPhases = this.phases
      }

      config.println(s"Phases = ${phases.deep}")
      config.println(s"nextDenotTransformerId = ${nextDenotTransformerId.deep}")
    }

    def phaseOfClass(pclass: Class[_]) = phases.find(pclass.isInstance).getOrElse(NoPhase)

    private val cachedPhases = collection.mutable.Set[PhaseCache]()
    private def cleanPhaseCache = cachedPhases.foreach(_.myPhase = NoPhase)

    /** A cache to compute the phase with given name, which
     *  stores the phase as soon as phaseNamed returns something
     *  different from NoPhase.
     */
    private class PhaseCache(pclass: Class[_ <: Phase]) {
      var myPhase: Phase = NoPhase
      def phase = {
        if (myPhase eq NoPhase) myPhase = phaseOfClass(pclass)
        myPhase
      }
      cachedPhases += this
    }

    private val typerCache = new PhaseCache(classOf[FrontEnd])
    private val picklerCache = new PhaseCache(classOf[Pickler])

    private val refChecksCache = new PhaseCache(classOf[RefChecks])
    private val elimRepeatedCache = new PhaseCache(classOf[ElimRepeated])
    private val extensionMethodsCache = new PhaseCache(classOf[ExtensionMethods])
    private val erasureCache = new PhaseCache(classOf[Erasure])
    private val elimErasedValueTypeCache = new PhaseCache(classOf[ElimErasedValueType])
    private val patmatCache = new PhaseCache(classOf[PatternMatcher])
    private val lambdaLiftCache = new PhaseCache(classOf[LambdaLift])
    private val flattenCache = new PhaseCache(classOf[Flatten])
    private val explicitOuterCache = new PhaseCache(classOf[ExplicitOuter])
    private val gettersCache = new PhaseCache(classOf[Getters])
    private val genBCodeCache = new PhaseCache(classOf[GenBCode])

    def typerPhase = typerCache.phase
    def picklerPhase = picklerCache.phase
    def refchecksPhase = refChecksCache.phase
    def elimRepeatedPhase = elimRepeatedCache.phase
    def extensionMethodsPhase = extensionMethodsCache.phase
    def erasurePhase = erasureCache.phase
    def elimErasedValueTypePhase = elimErasedValueTypeCache.phase
    def patmatPhase = patmatCache.phase
    def lambdaLiftPhase = lambdaLiftCache.phase
    def flattenPhase = flattenCache.phase
    def explicitOuterPhase = explicitOuterCache.phase
    def gettersPhase = gettersCache.phase
    def genBCodePhase = genBCodeCache.phase

    def isAfterTyper(phase: Phase): Boolean = phase.id > typerPhase.id
  }

  trait Phase extends DotClass {

    /** A name given to the `Phase` that can be used to debug the compiler. For
     *  instance, it is possible to print trees after a given phase using:
     *
     *  ```bash
     *  $ ./bin/dotc -Xprint:<phaseNameHere> sourceFile.scala
     *  ```
     */
    def phaseName: String

    def isRunnable(implicit ctx: Context): Boolean =
      !ctx.reporter.hasErrors

    /** List of names of phases that should precede this phase */
    def runsAfter: Set[Class[_ <: Phase]] = Set.empty

    /** @pre `isRunnable` returns true */
    def run(implicit ctx: Context): Unit

    /** @pre `isRunnable` returns true */
    def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
      units.map { unit =>
        val unitCtx = ctx.fresh.setPhase(this.start).setCompilationUnit(unit)
        run(unitCtx)
        unitCtx.compilationUnit
      }

    def description: String = phaseName

    /** Output should be checkable by TreeChecker */
    def isCheckable: Boolean = true

    /** Check what the phase achieves, to be called at any point after it is finished.
     */
    def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = ()

    /** If set, allow missing or superfluous arguments in applications
     *  and type applications.
     */
    def relaxedTyping: Boolean = false

    /** Is this phase the standard typerphase? True for FrontEnd, but
     *  not for other first phases (such as FromTasty). The predicate
     *  is tested in some places that perform checks and corrections. It's
     *  different from isAfterTyper (and cheaper to test).
     */
    def isTyper = false

    /** Can this transform create or delete non-private members? */
    def changesMembers: Boolean = false

    /** Can this transform change the parents of a class? */
    def changesParents: Boolean = false

    def exists: Boolean = true

    private var myPeriod: Period = Periods.InvalidPeriod
    private var myBase: ContextBase = null
    private var myErasedTypes = false
    private var myFlatClasses = false
    private var myRefChecked = false
    private var mySymbolicRefs = false
    private var myLabelsReordered = false

    private var mySameMembersStartId = NoPhaseId
    private var mySameParentsStartId = NoPhaseId

    /** The sequence position of this phase in the given context where 0
     * is reserved for NoPhase and the first real phase is at position 1.
     * -1 if the phase is not installed in the context.
     */
    def id = myPeriod.firstPhaseId

    def period = myPeriod
    def start = myPeriod.firstPhaseId
    def end = myPeriod.lastPhaseId

    final def erasedTypes = myErasedTypes   // Phase is after erasure
    final def flatClasses = myFlatClasses   // Phase is after flatten
    final def refChecked = myRefChecked     // Phase is after RefChecks
    final def symbolicRefs = mySymbolicRefs // Phase is after ResolveSuper, newly generated TermRefs should be symbolic
    final def labelsReordered = myLabelsReordered // Phase is after LabelDefs, labels are flattened and owner chains don't mirror this

    final def sameMembersStartId = mySameMembersStartId
      // id of first phase where all symbols are guaranteed to have the same members as in this phase
    final def sameParentsStartId = mySameParentsStartId
      // id of first phase where all symbols are guaranteed to have the same parents as in this phase

    protected[Phases] def init(base: ContextBase, start: Int, end:Int): Unit = {
      if (start >= FirstPhaseId)
        assert(myPeriod == Periods.InvalidPeriod, s"phase $this has already been used once; cannot be reused")
      assert(start <= Periods.MaxPossiblePhaseId, s"Too many phases, Period bits overflow")
      myBase = base
      myPeriod = Period(NoRunId, start, end)
      myErasedTypes  = prev.getClass == classOf[Erasure]      || prev.erasedTypes
      myFlatClasses  = prev.getClass == classOf[Flatten]      || prev.flatClasses
      myRefChecked   = prev.getClass == classOf[RefChecks]    || prev.refChecked
      mySymbolicRefs = prev.getClass == classOf[ResolveSuper] || prev.symbolicRefs
      myLabelsReordered = prev.getClass == classOf[LabelDefs] || prev.labelsReordered
      mySameMembersStartId = if (changesMembers) id else prev.sameMembersStartId
      mySameParentsStartId = if (changesParents) id else prev.sameMembersStartId
    }

    protected[Phases] def init(base: ContextBase, id: Int): Unit = init(base, id, id)

    final def <=(that: Phase) =
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

  trait NeedsCompanions {
    def isCompanionNeeded(cls: ClassSymbol)(implicit ctx: Context): Boolean
  }

  /** Replace all instances of `oldPhaseClass` in `current` phases
   *  by the result of `newPhases` applied to the old phase.
   */
  def replace(oldPhaseClass: Class[_ <: Phase], newPhases: Phase => List[Phase], current: List[List[Phase]]): List[List[Phase]] =
    current.map(_.flatMap(phase =>
      if (oldPhaseClass.isInstance(phase)) newPhases(phase) else phase :: Nil))

  /** Dotty deviation: getClass yields Class[_], instead of [Class <: <type of receiver>].
   *  We can get back the old behavior using this decorator. We should also use the same
   *  trick for standard getClass.
   */
  private implicit class getClassDeco[T](val x: T) extends AnyVal {
    def getClazz: Class[_ <: T] = x.getClass.asInstanceOf[Class[_ <: T]]
  }
}
