package dotty.tools
package dotc
package core

import Periods._
import Contexts._
import dotty.tools.backend.jvm.GenBCode
import DenotTransformers._
import Denotations._
import Decorators._
import config.Printers.config
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.transform.MegaPhase._
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
    def allPhases: Array[Phase] = (if (squashedPhases.nonEmpty) squashedPhases else phases).tail

    object NoPhase extends Phase {
      override def exists: Boolean = false
      def phaseName: String = "<no phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = unsupported("transform")
    }

    object SomePhase extends Phase {
      def phaseName: String = "<some phase>"
      def run(implicit ctx: Context): Unit = unsupported("run")
    }

    /** A sentinel transformer object */
    class TerminalPhase extends DenotTransformer {
      def phaseName: String = "terminal"
      def run(implicit ctx: Context): Unit = unsupported("run")
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
        unsupported("transform")
      override def lastPhaseId(implicit ctx: Context): Int = id
    }

    final def phasePlan: List[List[Phase]] = this.phasesPlan
    final def setPhasePlan(phasess: List[List[Phase]]): Unit = this.phasesPlan = phasess

    /** Squash TreeTransform's beloning to same sublist to a single TreeTransformer
      * Each TreeTransform gets own period,
      * whereas a combined TreeTransformer gets period equal to union of periods of it's TreeTransforms
      */
    final def squashPhases(phasess: List[List[Phase]],
                           phasesToSkip: List[String],
                           stopBeforePhases: List[String],
                           stopAfterPhases: List[String],
                           YCheckAfter: List[String])(implicit ctx: Context): List[Phase] = {
      val squashedPhases = ListBuffer[Phase]()
      var prevPhases: Set[String] = Set.empty
      val YCheckAll = YCheckAfter.contains("all")

      var stop = false

      def isEnabled(p: Phase): Boolean =
        !stop &&
        !stopBeforePhases.contains(p.phaseName) &&
        !phasesToSkip.contains(p.phaseName) &&
        p.isEnabled

      val filteredPhases = phasess.map(_.filter { p =>
        try isEnabled(p)
        finally stop |= stopBeforePhases.contains(p.phaseName) | stopAfterPhases.contains(p.phaseName)
      })

      var i = 0

      while (i < filteredPhases.length) {
        if (filteredPhases(i).nonEmpty) { //could be empty due to filtering
          val filteredPhaseBlock = filteredPhases(i)
          val phaseToAdd =
            if (filteredPhaseBlock.length > 1) {
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
              val superPhase = new MegaPhase(filteredPhaseBlock.asInstanceOf[List[MiniPhase]].toArray)
              prevPhases ++= filteredPhaseBlock.map(_.phaseName)
              superPhase
            } else { // block of a single phase, no squashing
              val phase = filteredPhaseBlock.head
              prevPhases += phase.phaseName
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
    final def usePhases(phasess: List[Phase], squash: Boolean = true): Unit = {

      val flatPhases = collection.mutable.ListBuffer[Phase]()

      phasess.foreach(p => p match {
        case p: MegaPhase => flatPhases ++= p.miniPhases
        case _ => flatPhases += p
      })

      phases = (NoPhase :: flatPhases.toList ::: new TerminalPhase :: Nil).toArray
      setSpecificPhases()
      var phasesAfter: Set[String] = Set.empty
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
        phasesAfter += p.phaseName

      }
      var i = 0

      while (i < phasess.length) {
        val phase = phasess(i)
        phase match {
          case p: MegaPhase =>
            val miniPhases = p.miniPhases
            miniPhases.foreach{ phase =>
              checkRequirements(phase)
              phase.init(this, nextPhaseId)}
            p.init(this, miniPhases.head.id, miniPhases.last.id)
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

    private[this] var myTyperPhase: Phase = _
    private[this] var myPostTyperPhase: Phase = _
    private[this] var mySbtExtractDependenciesPhase: Phase = _
    private[this] var myPicklerPhase: Phase = _
    private[this] var myReifyQuotesPhase: Phase = _
    private[this] var myCollectNullableFieldsPhase: Phase = _
    private[this] var myRefChecksPhase: Phase = _
    private[this] var myPatmatPhase: Phase = _
    private[this] var myElimRepeatedPhase: Phase = _
    private[this] var myExtensionMethodsPhase: Phase = _
    private[this] var myExplicitOuterPhase: Phase = _
    private[this] var myGettersPhase: Phase = _
    private[this] var myErasurePhase: Phase = _
    private[this] var myElimErasedValueTypePhase: Phase = _
    private[this] var myLambdaLiftPhase: Phase = _
    private[this] var myFlattenPhase: Phase = _
    private[this] var myGenBCodePhase: Phase = _

    final def typerPhase: Phase = myTyperPhase
    final def postTyperPhase: Phase = myPostTyperPhase
    final def sbtExtractDependenciesPhase: Phase = mySbtExtractDependenciesPhase
    final def picklerPhase: Phase = myPicklerPhase
    final def reifyQuotesPhase: Phase = myReifyQuotesPhase
    final def collectNullableFieldsPhase: Phase = myCollectNullableFieldsPhase
    final def refchecksPhase: Phase = myRefChecksPhase
    final def patmatPhase: Phase = myPatmatPhase
    final def elimRepeatedPhase: Phase = myElimRepeatedPhase
    final def extensionMethodsPhase: Phase = myExtensionMethodsPhase
    final def explicitOuterPhase: Phase = myExplicitOuterPhase
    final def gettersPhase: Phase = myGettersPhase
    final def erasurePhase: Phase = myErasurePhase
    final def elimErasedValueTypePhase: Phase = myElimErasedValueTypePhase
    final def lambdaLiftPhase: Phase = myLambdaLiftPhase
    final def flattenPhase: Phase = myFlattenPhase
    final def genBCodePhase: Phase = myGenBCodePhase

    private def setSpecificPhases() = {
      def phaseOfClass(pclass: Class[_]) = phases.find(pclass.isInstance).getOrElse(NoPhase)

      myTyperPhase = phaseOfClass(classOf[FrontEnd])
      myPostTyperPhase = phaseOfClass(classOf[PostTyper])
      mySbtExtractDependenciesPhase = phaseOfClass(classOf[sbt.ExtractDependencies])
      myPicklerPhase = phaseOfClass(classOf[Pickler])
      myReifyQuotesPhase = phaseOfClass(classOf[ReifyQuotes])
      myCollectNullableFieldsPhase = phaseOfClass(classOf[CollectNullableFields])
      myRefChecksPhase = phaseOfClass(classOf[RefChecks])
      myElimRepeatedPhase = phaseOfClass(classOf[ElimRepeated])
      myExtensionMethodsPhase = phaseOfClass(classOf[ExtensionMethods])
      myErasurePhase = phaseOfClass(classOf[Erasure])
      myElimErasedValueTypePhase = phaseOfClass(classOf[ElimErasedValueType])
      myPatmatPhase = phaseOfClass(classOf[PatternMatcher])
      myLambdaLiftPhase = phaseOfClass(classOf[LambdaLift])
      myFlattenPhase = phaseOfClass(classOf[Flatten])
      myExplicitOuterPhase = phaseOfClass(classOf[ExplicitOuter])
      myGettersPhase = phaseOfClass(classOf[Getters])
      myGenBCodePhase =  phaseOfClass(classOf[GenBCode])
    }

    final def isAfterTyper(phase: Phase): Boolean = phase.id > typerPhase.id
  }

  abstract class Phase {

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
        // TODO: This might test an unintended condition.
        // To find out whether any errors have been reported during this
        // run one calls `errorsReported`, not `hasErrors`.
        // But maybe changing this would prevent useful phases from running?

    /** If set, allow missing or superfluous arguments in applications
     *  and type applications.
     */
    def relaxedTyping: Boolean = false

    /** If set, implicit search is enabled */
    def allowsImplicitSearch: Boolean = false

     /** List of names of phases that should precede this phase */
    def runsAfter: Set[String] = Set.empty

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

    /** Is this phase the standard typerphase? True for FrontEnd, but
     *  not for other first phases (such as FromTasty). The predicate
     *  is tested in some places that perform checks and corrections. It's
     *  different from isAfterTyper (and cheaper to test).
     */
    def isTyper: Boolean = false

    /** Can this transform create or delete non-private members? */
    def changesMembers: Boolean = false

    /** Can this transform change the parents of a class? */
    def changesParents: Boolean = false

    /** Can this transform change the base types of a type? */
    def changesBaseTypes: Boolean = changesParents

    def isEnabled(implicit ctx: Context): Boolean = true

    def exists: Boolean = true

    def initContext(ctx: FreshContext): Unit = ()

    private[this] var myPeriod: Period = Periods.InvalidPeriod
    private[this] var myBase: ContextBase = null
    private[this] var myErasedTypes = false
    private[this] var myFlatClasses = false
    private[this] var myRefChecked = false

    private[this] var mySameMembersStartId = NoPhaseId
    private[this] var mySameParentsStartId = NoPhaseId
    private[this] var mySameBaseTypesStartId = NoPhaseId

    /** The sequence position of this phase in the given context where 0
     * is reserved for NoPhase and the first real phase is at position 1.
     * -1 if the phase is not installed in the context.
     */
    def id: Int = myPeriod.firstPhaseId

    def period: Period = myPeriod
    def start: Int = myPeriod.firstPhaseId
    def end: Periods.PhaseId = myPeriod.lastPhaseId

    final def erasedTypes: Boolean = myErasedTypes   // Phase is after erasure
    final def flatClasses: Boolean = myFlatClasses   // Phase is after flatten
    final def refChecked: Boolean = myRefChecked     // Phase is after RefChecks

    final def sameMembersStartId: Int = mySameMembersStartId
      // id of first phase where all symbols are guaranteed to have the same members as in this phase
    final def sameParentsStartId: Int = mySameParentsStartId
      // id of first phase where all symbols are guaranteed to have the same parents as in this phase
    final def sameBaseTypesStartId: Int = mySameBaseTypesStartId
      // id of first phase where all symbols are guaranteed to have the same base tpyes as in this phase

    protected[Phases] def init(base: ContextBase, start: Int, end: Int): Unit = {
      if (start >= FirstPhaseId)
        assert(myPeriod == Periods.InvalidPeriod, s"phase $this has already been used once; cannot be reused")
      assert(start <= Periods.MaxPossiblePhaseId, s"Too many phases, Period bits overflow")
      myBase = base
      myPeriod = Period(NoRunId, start, end)
      myErasedTypes  = prev.getClass == classOf[Erasure]   || prev.erasedTypes
      myFlatClasses  = prev.getClass == classOf[Flatten]   || prev.flatClasses
      myRefChecked   = prev.getClass == classOf[RefChecks] || prev.refChecked
      mySameMembersStartId = if (changesMembers) id else prev.sameMembersStartId
      mySameParentsStartId = if (changesParents) id else prev.sameParentsStartId
      mySameBaseTypesStartId = if (changesBaseTypes) id else prev.sameBaseTypesStartId
    }

    protected[Phases] def init(base: ContextBase, id: Int): Unit = init(base, id, id)

    final def <=(that: Phase): Boolean =
      exists && id <= that.id

    final def prev: Phase =
      if (id > FirstPhaseId) myBase.phases(start - 1) else myBase.NoPhase

    final def next: Phase =
      if (hasNext) myBase.phases(end + 1) else myBase.NoPhase

    final def hasNext: Boolean = start >= FirstPhaseId && end + 1 < myBase.phases.length

    final def iterator: Iterator[Phase] =
      Iterator.iterate(this)(_.next) takeWhile (_.hasNext)

    override def toString: String = phaseName
  }

  /** Replace all instances of `oldPhaseClass` in `current` phases
   *  by the result of `newPhases` applied to the old phase.
   */
  def replace(oldPhaseClass: Class[_ <: Phase], newPhases: Phase => List[Phase], current: List[List[Phase]]): List[List[Phase]] =
    current.map(_.flatMap(phase =>
      if (oldPhaseClass.isInstance(phase)) newPhases(phase) else phase :: Nil))
}
