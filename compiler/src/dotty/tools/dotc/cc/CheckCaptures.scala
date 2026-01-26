package dotty.tools
package dotc
package cc

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import config.Printers.{capt, recheckr, noPrinter}
import config.{Config, Feature}
import ast.{tpd, untpd, Trees}
import Trees.*
import typer.ForceDegree
import typer.Inferencing.isFullyDefined
import typer.RefChecks.{checkAllOverrides, checkSelfAgainstParents, OverridingPairsChecker}
import typer.Checking.{checkBounds, checkAppliedTypesIn}
import typer.ErrorReporting.err
import typer.ProtoTypes.{LhsProto, WildcardSelectionProto, SelectionProto}
import util.{SimpleIdentitySet, EqHashMap, EqHashSet, SrcPos, Property}
import util.chaining.tap
import transform.{Recheck, PreRecheck, CapturedVars}
import Recheck.*
import scala.collection.mutable
import CaptureSet.{withCaptureSetsExplained, IncludeFailure, MutAdaptFailure, VarState}
import CCState.*
import StdNames.nme
import NameKinds.{DefaultGetterName, WildcardParamName, UniqueNameKind}
import reporting.{trace, Message, OverrideError}
import reporting.Message.Note
import Annotations.Annotation
import Capabilities.*
import Mutability.*
import TypeOps.AsSeenFromMap
import util.common.alwaysTrue
import scala.annotation.constructorOnly

/** The capture checker */
object CheckCaptures:
  import ast.tpd.*

  val name: String = "cc"
  val description: String = "capture checking"

  enum EnvKind derives CanEqual:
    case Regular        // normal case
    case NestedInOwner  // environment is a temporary one nested in the owner's environment,
                        // and does not have a different actual owner symbol
                        // (this happens when doing box adaptation).
    case Boxed          // environment is inside a box (in which case references are not counted)

  /** A class describing environments.
   *  @param owner         the current owner
   *  @param kind          the environment's kind
   *  @param captured      the capture set containing all references to tracked free variables outside of boxes
   *  @param outer0        the next enclosing environment
   *  @param nestedClosure under deferredReaches: If this is an env of a method with an anonymous function or
   *                       anonymous class as RHS, the symbol of that function or class. NoSymbol in all other cases.
   */
  class Env(
    val owner: Symbol,
    val kind: EnvKind,
    val captured: CaptureSet,
    outer0: Env | Null,
    val nestedClosure: Symbol = NoSymbol)(using @constructorOnly ictx: Context) {

    assert(definesEnv(owner))
    captured match
      case captured: CaptureSet.Var => assert(captured.owner == owner,
        i"owner discrepancy env owner = $owner but its captureset $captured has owner ${captured.owner}")
      case _ =>

    def outer = outer0.nn

    def isRoot(using Context) = owner.is(Package)

    def outersIterator(using Context): Iterator[Env] = new:
      private var cur = Env.this
      def hasNext = !cur.isRoot
      def next(): Env =
        val res = cur
        cur = cur.outer
        res
  }

  def definesEnv(sym: Symbol)(using Context): Boolean =
    sym.isOneOf(MethodOrLazy) || sym.isClass

  /** Similar normal substParams, but this is an approximating type map that
   *  maps parameters in contravariant capture sets to the empty set.
   */
  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context)
  extends ApproximatingTypeMap:
    def apply(tp: Type): Type =
      tp match
        case tp: ParamRef =>
          if tp.binder == from then to(tp.paramNum) else tp
        case tp: NamedType =>
          if tp.prefix `eq` NoPrefix then tp
          else tp.derivedSelect(apply(tp.prefix))
        case _: ThisType =>
          tp
        case _ =>
          mapOver(tp)
    override def toString = "SubstParamsMap"
  end SubstParamsMap

  /** Check that a @retains annotation only mentions references that can be tracked.
   *  This check is performed at Typer.
   */
  def checkWellformedRetains(parent: Tree, ann: Tree)(using Context): Unit =
    def check(elem: Type): Unit = elem match
      case ref: TypeRef =>
        val refSym = ref.symbol
        if refSym.isType && !refSym.info.derivesFrom(defn.Caps_CapSet) then
          report.error(em"$elem is not a legal element of a capture set", ann.srcPos)
      case ref: CoreCapability =>
        if !ref.isTrackableRef && !ref.isLocalMutable then
          report.error(em"$elem cannot be tracked since it is not a parameter or local value", ann.srcPos)
      case ReachCapability(ref) =>
        check(ref)
        if ref.isCapsAnyRef || ref.isCapsFreshRef then
          report.error(em"Cannot form a reach capability from `${ref.termSymbol.name}`", ann.srcPos)
      case ReadOnlyCapability(ref) =>
        check(ref)
      case OnlyCapability(ref, cls) =>
        if !cls.isClassifiedCapabilityClass then
          report.error(
            em"""${ref.showRef}.only[${cls.name}] is not well-formed since $cls is not a classifier class.
                |A classifier class is a class extending `caps.Capability` and directly extending `caps.Classifier`.""",
            ann.srcPos)
        check(ref)
      case elem =>
        report.error(em"$elem is not a legal element of a capture set", ann.srcPos)
    ann.retainedSet.retainedElementsRaw.foreach(check)

  /** Disallow bad roots anywhere in type `tp``.
   *  @param  upto  controls up to which owner local LocalCap capabilities should be disallowed.
   *                See disallowBadRoots for details.
   */
  private def disallowBadRootsIn(tp: Type, upto: Symbol, what: => String, have: => String, addendum: => String, pos: SrcPos)(using Context) =
    val check = new TypeTraverser:

      private val seen = new EqHashSet[TypeRef]

      // We keep track of open existential scopes here so that we can set these scopes
      // in ccState when printing a part of the offending type.
      var openExistentialScopes: List[MethodType] = Nil

      def traverse(t: Type) =
        t.dealiasKeepAnnots match
          case t: TypeRef =>
            if !seen.contains(t) then
              seen += t
              traverseChildren(t)

              // Check the lower bound of path dependent types.
              // See issue #19330.
              val isMember = t.prefix ne NoPrefix
              t.info match
                case TypeBounds(lo, _) if isMember => traverse(lo)
                case _ =>
          case AnnotatedType(_, ann) if ann.symbol == defn.UncheckedCapturesAnnot =>
            ()
          case CapturingType(parent, refs) =>
            if variance >= 0 then
              val openScopes = openExistentialScopes
              refs.disallowBadRoots(upto): () =>
                def part =
                  if t eq tp then ""
                  else
                    // Show in context of all enclosing traversed existential scopes.
                    def showInOpenedResultBinders(mts: List[MethodType]): String = mts match
                      case Nil => i"the part $t of "
                      case mt :: mts1 =>
                        inNewExistentialScope(mt):
                          showInOpenedResultBinders(mts1)
                    showInOpenedResultBinders(openScopes.reverse)
                report.error(
                  em"""$what cannot $have $tp since
                      |${part}that type captures the root capability `any`.$addendum""",
                  pos)
            traverse(parent)
          case defn.RefinedFunctionOf(mt) =>
            traverse(mt)
          case t: MethodType if t.marksExistentialScope =>
            atVariance(-variance):
              t.paramInfos.foreach(traverse)
            val saved = openExistentialScopes
            openExistentialScopes = t :: openExistentialScopes
            try traverse(t.resType)
            finally openExistentialScopes = saved
          case t =>
            traverseChildren(t)
    check.traverse(tp)
  end disallowBadRootsIn

  private def contributesLocalCapToClass(sym: Symbol)(using Context): Boolean =
    sym.isField
    && !sym.isOneOf(DeferredOrTermParamOrAccessor)
    && !sym.hasAnnotation(defn.UntrackedCapturesAnnot)

  trait CheckerAPI:
    /** Complete symbol info of a val or a def */
    def completeDef(tree: ValOrDefDef, sym: Symbol, completer: LazyType)(using Context): Type

    extension [T <: Tree](tree: T)

      /** Set new type of the tree if none was installed yet. */
      def setNuType(tpe: Type): Unit

      /** The new type of the tree, or if none was installed, the original type */
      def nuType(using Context): Type

      /** Was a new type installed for this tree? */
      def hasNuType: Boolean

      /** Is this tree passed to a parameter or assigned to a value with a type
       *  that contains `any` in no-flip covariant position, which will necessite
       *  a separation check?
       */
      def needsSepCheck: Boolean

      /** If a tree is an argument for which needsSepCheck is true,
       *  the type of the formal parameter corresponding to the argument.
       */
      def formalType: Type

      /** The "use set", i.e. the capture set marked as free at this node. */
      def markedFree: CaptureSet

  end CheckerAPI

class CheckCaptures extends Recheck, SymTransformer:
  thisPhase =>

  import ast.tpd.*
  import CheckCaptures.*

  override def phaseName: String = CheckCaptures.name

  override def description: String = CheckCaptures.description

  override def isRunnable(using Context) = super.isRunnable && Feature.ccEnabledSomewhere

  /** We normally need a recompute if the prefix is a SingletonType and the
   *  last denotation is not a SymDenotation. The SingletonType requirement is
   *  so that we don't widen TermRefs with non-path prefixes to their underlying
   *  type when recomputing their denotations with asSeenFrom. Such widened types
   *  would become illegal members of capture sets.
   *
   *  The SymDenotation requirement is so that we don't recompute termRefs of Symbols
   *  which should be handled by SymTransformers alone. However, if the underlying type
   *  of the prefix is a capturing type, we do need to recompute since in that case
   *  the prefix might carry a parameter refinement created in Setup, and we need to
   *  take these refinements into account.
   */
  override def needsRecompute(tp: NamedType, lastDenotation: SingleDenotation)(using Context): Boolean =
    tp.prefix match
      case prefix: TermRef =>
        !lastDenotation.isInstanceOf[SymDenotation]
        || !prefix.info.captureSet.isAlwaysEmpty
      case prefix: SingletonType =>
        !lastDenotation.isInstanceOf[SymDenotation]
      case _ =>
        false

  def newRechecker()(using Context) = CaptureChecker(ctx)

  override def run(using Context): Unit =
    if Feature.ccEnabled then
      super.run

  val ccState1 = new CCState // Dotty problem: Rename to ccState ==> Crash in ExplicitOuter

  /** A cache that stores for each class the classifiers of all LocalCap instances
   *  in the types of its fields.
   */
  val knownLocalCapClassifiers = new util.EqHashMap[Symbol, List[ClassSymbol]]

  class CaptureChecker(ictx: Context) extends Rechecker(ictx), CheckerAPI:

    // println(i"checking ${ictx.source}"(using ictx))

    /** The current environment */
    private val rootEnv: Env = inContext(ictx):
      Env(defn.RootClass, EnvKind.Regular, CaptureSet.empty, null)
    private var curEnv = rootEnv

    /** Currently checked closures and their expected types, used for error reporting */
    private var openClosures: List[(Symbol, Type)] = Nil

    private val myCapturedVars: util.EqHashMap[Symbol, CaptureSet] = EqHashMap()

    /** A list of actions to perform at postCheck. The reason to defer these actions
     *  is that it is sometimes better for type inference to not constrain too early
     *  with a checkConformsExpr.
     */
    private val todoAtPostCheck = new mutable.ListBuffer[() => Unit]

    /** Maps trees that need a separation check because they are arguments to
     *  polymorphic parameters. The trees are mapped to the formal parameter type.
     */
    private val sepCheckFormals = util.EqHashMap[Tree, Type]()

    /** The references used at identifier or application trees, including the
     *  environment at the reference point.
     */
    private val useInfos = mutable.ArrayBuffer[(Tree, CaptureSet, Env)]()

    private val usedSet = util.EqHashMap[Tree, CaptureSet]()

    /** The set of symbols that were rechecked via a completer */
    private val completed = new mutable.HashSet[Symbol]

    /** Set on recheckClassDef since there we see all language imports */
    private var sepChecksEnabled = false

    private var needAnotherRun = false

    def resetIteration()(using Context): Unit =
      needAnotherRun = false
      resetNuTypes()
      todoAtPostCheck.clear()
      completed.clear()

    extension [T <: Tree](tree: T)
      def needsSepCheck: Boolean = sepCheckFormals.contains(tree)
      def formalType: Type = sepCheckFormals.getOrElse(tree, NoType)
      def markedFree: CaptureSet = usedSet.getOrElse(tree, CaptureSet.empty)

    /** Instantiate capture set variables appearing contra-variantly to their
     *  upper approximation.
     */
    private def interpolate(tp: Type, sym: Symbol, startingVariance: Int = 1)(using Context): Unit =

      object variances extends TypeTraverser:
        variance = startingVariance
        val varianceOfVar = EqHashMap[CaptureSet.Var, Int]()
        override def traverse(t: Type) = t match
          case t @ CapturingType(parent, refs) =>
            refs match
              case refs: CaptureSet.Var if !refs.isConst =>
                varianceOfVar(refs) = varianceOfVar.get(refs) match
                  case Some(v0) => if v0 == 0 then 0 else (v0 + variance) / 2
                  case None => variance
              case _ =>
            traverse(parent)
          case t @ defn.RefinedFunctionOf(rinfo) =>
            traverse(rinfo)
          case _ =>
            traverseChildren(t)

      val interpolator = new TypeTraverser:
        override def traverse(t: Type) = t match
          case t @ CapturingType(parent, refs) =>
            refs match
              case refs: CaptureSet.Var if !refs.isConst =>
                if variances.varianceOfVar(refs) < 0 then refs.solve()
                else refs.markSolved(provisional = !sym.isMutableVar)
              case _ =>
            traverse(parent)
          case t @ defn.RefinedFunctionOf(rinfo) =>
            traverse(rinfo)
          case _ =>
            traverseChildren(t)

      variances.traverse(tp)
      interpolator.traverse(tp)
    end interpolate

    /*  Also set any previously unset owners of toplevel LocalCap instances to improve
     *  error diagnostics in separation checking.
     */
    private def anchorCaps(sym: Symbol)(using Context) = new TypeTraverser:
      override def traverse(t: Type) =
        if variance > 0 then
          t match
            case t @ CapturingType(parent, refs) =>
              for ref <- refs.elems do
                ref match
                  case ref: LocalCap if !ref.hiddenSet.givenOwner.exists =>
                    ref.hiddenSet.givenOwner = sym
                  case _ =>
              traverse(parent)
            case t @ defn.RefinedFunctionOf(rinfo) =>
              traverse(rinfo)
            case _ =>
              traverseChildren(t)

    /** If `tpt` is an inferred type, interpolate capture set variables appearing contra-
     *  variantly in it. Also anchor LocalCap instances with anchorCaps.
     *  Note: module vals don't have inferred types but still hold capture set variables.
     *  These capture set variables are interpolated after the associated module class
     *  has been rechecked.
     */
    private def interpolateIfInferred(tpt: Tree, sym: Symbol)(using Context): Unit =
      if tpt.isInstanceOf[InferredTypeTree] then
        interpolate(tpt.nuType, sym)
          .showing(i"solved vars for $sym in ${tpt.nuType}", capt)
        anchorCaps(sym).traverse(tpt.nuType)
        for msg <- ccState.approxWarnings do
          report.warning(msg, tpt.srcPos)
        ccState.approxWarnings.clear()

    /** Assert subcapturing `cs1 <: cs2` (available for debugging, otherwise unused) */
    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert(cs1.subCaptures(cs2), i"$cs1 is not a subset of $cs2")

    /** If `res` is not CompareResult.OK, report an error */
    def checkOK(res: TypeComparer.CompareResult,
        prefix: => String,
        added: Capability | CaptureSet,
        target: CaptureSet,
        pos: SrcPos,
        provenance: => String = "")(using Context): Unit =
      res match
        case TypeComparer.CompareResult.Fail(notes) =>
          val (includeFailures, otherNotes) = notes.partition(_.isInstanceOf[IncludeFailure])
          val realTarget = includeFailures match
            case (fail: IncludeFailure) :: _ => fail.cs
            case _ => target
          def msg(provisional: Boolean) =
            def toAdd: String = otherNotes.map(_.render).mkString
            def descr: String =
              val d = realTarget.description
              if d.isEmpty then provenance else ""
            def kind = if provisional then "previously estimated\n" else "allowed "
            em"$prefix included in the ${kind}capture set $realTarget$descr$toAdd"
          target match
            case target: CaptureSet.Var
            if realTarget.isProvisionallySolved =>
              report.warning(
                msg(provisional = true)
                  .prepend(i"Another capture checking run needs to be scheduled because\n"),
                pos)
              needAnotherRun = true
              added match
                case added: Capability => target.elems += added
                case added: CaptureSet => target.elems ++= added.elems
            case _ =>
              report.error(msg(provisional = false), pos)
        case _ =>

    /** Check subcapturing `{elem} <: cs`, report error on failure */
    def checkElem(elem: Capability, cs: CaptureSet, pos: SrcPos, provenance: => String = "")(using Context) =
      checkOK(
          TypeComparer.compareResult(elem.singletonCaptureSet.subCaptures(cs)),
          i"$elem cannot be referenced here; it is not",
          elem, cs, pos, provenance)

    /** Check subcapturing `cs1 <: cs2`, report error on failure */
    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos,
        provenance: => String = "", cs1description: String = "")(using Context) =
      checkOK(
          TypeComparer.compareResult(cs1.subCaptures(cs2)),
          if cs1.elems.size == 1 then
            i"reference `${cs1.elems.nth(0).showAsCapability}`$cs1description is not"
          else
            i"references $cs1$cs1description are not all",
          cs1, cs2, pos, provenance)

    /** If `sym` is a method or a non-static inner class, a capture set variable
     *  representing the captured variables of the environment associated with `sym`.
     */
    def capturedVars(sym: Symbol)(using Context): CaptureSet =
      myCapturedVars.getOrElseUpdate(sym,
        sym.getAnnotation(defn.RetainsAnnot) match
          case Some(ann: RetainingAnnotation) =>
            try ann.toCaptureSet
            catch case ex: IllegalCaptureRef =>
              report.error(em"Illegal capture reference: ${ex.getMessage}", sym.srcPos)
              CaptureSet.empty
          case _ if sym.is(Package) => CaptureSet.empty
          case _ => CaptureSet.Var(sym, nestedOK = false)
      )

// ---- Record Uses with MarkFree ----------------------------------------------------

    /** The next environment enclosing `env` that needs to be charged
     *  with free references.
     */
    def nextEnvToCharge(env: Env)(using Context): Env =
      if env.owner.isConstructor then env.outer.outer
      else env.outer

    /** A description where this environment comes from */
    private def provenance(env: Env)(using Context): String =
      val owner = env.owner
      if owner.isAnonymousFunction then
        val expected = openClosures
          .find(_._1 == owner)
          .map(_._2)
          .getOrElse(owner.info.toFunctionType())
        i"\nof an enclosing function literal with expected type $expected"
      else
        i"\nof the enclosing ${owner.showLocated}"

    /** Under deferredReaches:
     *  Does the given environment belong to a method that is (a) nested in a term
     *  and (b) not the method of an anonymous function?
     */
    def isOfNestedMethod(env: Env)(using Context) =
      ccConfig.deferredReaches
      && env.owner.is(Method)
      && env.owner.owner.isTerm
      && !env.owner.isAnonymousFunction

    /** Include `sym` in the capture sets of all enclosing environments nested in the
     *  the environment in which `sym` is defined.
     */
    def markFree(sym: Symbol, tree: Tree)(using Context): Unit =
      markFree(sym, sym.termRef, tree)

    def markFree(sym: Symbol, ref: Capability, tree: Tree)(using Context): Unit =
      if sym.exists then markFree(ref, tree)

    def markFree(ref: Capability, tree: Tree)(using Context): Unit =
      if ref.isTracked then markFree(ref.singletonCaptureSet, tree)

    /** Make sure the (projected) `cs` is a subset of the capture sets of all enclosing
     *  environments. At each stage, only include references from `cs` that are outside
     *  the environment's owner
     */
    def markFree(cs: CaptureSet, tree: Tree, addUseInfo: Boolean = true)(using Context): Unit =
      // A captured reference with the symbol `sym` is visible from the environment
      // if `sym` is not defined inside the owner of the environment.
      inline def isVisibleFromEnv(sym: Symbol, env: Env) =
        sym.exists && {
          val effectiveOwner =
            if env.owner.isConstructor then env.owner.owner
            else env.owner
          if env.kind == EnvKind.NestedInOwner then
            !sym.isProperlyContainedIn(effectiveOwner)
          else
            !sym.isContainedIn(effectiveOwner)
        }

      /** Avoid locally defined capability by charging the underlying type
       *  (which may not be `any`). This scheme applies only under the deferredReaches setting.
       */
      def avoidLocalCapability(c: Capability, env: Env, lastEnv: Env | Null): Unit =
        if c.isParamPath then
          c match
            case Reach(_) | _: TypeRef =>
              val accessFromNestedClosure =
                lastEnv != null && env.nestedClosure.exists && env.nestedClosure == lastEnv.owner
              if !accessFromNestedClosure then
                checkUseDeclared(c, tree.srcPos)
            case _ =>
        else
          val underlying = c match
            case Reach(c1) => CaptureSet.ofTypeDeeply(c1.widen)
            case _ => c.core match
              case c1: RootCapability => c1.singletonCaptureSet
              case c1: CoreCapability =>
                CaptureSet.ofType(c1.widen, followResult = ccConfig.useSpanCapset)
          capt.println(i"Widen reach $c to $underlying in ${env.owner}")
          underlying.disallowBadRoots(NoSymbol): () =>
            report.error(em"Local capability `${c.showAsCapability}`${env.owner.qualString("in")} cannot have `any` as underlying capture set", tree.srcPos)
          recur(underlying, env, lastEnv)

      /** Avoid locally defined capability if it is a reach capability or capture set
       *  parameter. This is the default.
       */
      def avoidLocalReachCapability(c: Capability, env: Env): Unit = c match
        case Reach(c1) if !c1.isParamPath =>
          // Parameter reaches are rejected in checkEscapingUses.
          // When a reach capabilty x* where `x` is not a parameter goes out
          // of scope, we need to continue with `x`'s underlying deep capture set.
          // It is an error if that set contains `any`.
          // The same is not an issue for normal capabilities since in a local
          // definition `val x = e`, the capabilities of `e` have already been charged.
          // Note: It's not true that the underlying capture set of a reach capability
          // is always `any`. Reach capabilities over paths depend on the prefix, which
          // might turn an `any` into something else.
          // The path-use.scala neg test contains an example.
          val underlying = CaptureSet.ofTypeDeeply(c1.widen)
          capt.println(i"Widen reach $c to $underlying in ${env.owner}")
          recur(underlying.filter(!_.isTerminalCapability), env, null)
            // We don't want to disallow underlying LocalCap instances, since these are
            // typically locally created LocalCap capabilities. We do check in checkEscapingUses
            // that they don't hide any parameter reach caps.
        case _ =>

      def checkReadOnlyMethod(included: CaptureSet, meth: Symbol): Unit =
        included.checkAddedElems: elem =>
          if elem.isExclusive() then
            report.error(
                em"""Read-only $meth accesses exclusive capability `${elem.showAsCapability}`;
                    |$meth should be declared an update method to allow this.""",
                tree.srcPos)

      def recur(cs: CaptureSet, env: Env, lastEnv: Env | Null): Unit =
        if env.kind != EnvKind.Boxed && !cs.isAlwaysEmpty then
          // Only captured references that are visible from the environment
          // should be included.
          val included = cs.filter: c =>
            val isVisible = isVisibleFromEnv(c.pathOwner, env)
            if !isVisible then
              if ccConfig.deferredReaches
              then avoidLocalCapability(c, env, lastEnv)
              else avoidLocalReachCapability(c, env)
            isVisible
          checkSubset(included, env.captured, tree.srcPos, provenance(env))
          capt.println(i"Include call or box capture $included from $cs in ${env.owner} --> ${env.captured}")
          if !isOfNestedMethod(env) then
            val nextEnv = nextEnvToCharge(env)
            if !nextEnv.isRoot then
              if nextEnv.owner != env.owner
                  && env.owner.isReadOnlyMember
                  && env.owner.owner.derivesFrom(defn.Caps_Stateful)
              then
                checkReadOnlyMethod(included, env.owner)
              recur(included, nextEnv, env)
          	// Under deferredReaches, don't propagate out of methods inside terms.
          	// The use set of these methods will be charged when that method is called.

      if !cs.isAlwaysEmpty && !CCState.discardUses then
        recur(cs, curEnv, null)
        if addUseInfo then useInfos += ((tree, cs, curEnv))
    end markFree

    /** Include references captured by the called method in the current environment stack */
    def includeCallCaptures(sym: Symbol, resType: Type, tree: Tree)(using Context): Unit = resType match
      case _: MethodOrPoly => // wait until method is fully applied
      case _ =>
        def isRetained(ref: Capability): Boolean = ref.pathRoot match
          case root: ThisType => ctx.owner.isContainedIn(root.cls)
          case _ => true
        if sym.exists && curEnv.kind != EnvKind.Boxed then
          var locals = capturedVars(sym)
          if sym.isConstructor then
            locals = mapClassCaptures(sym.owner.asClass, resType, locals)
          markFree(locals.filter(isRetained), tree)

    /** Type arguments come either from a TypeApply node or from an AppliedType
     *  which represents a trait parent in a template.
     *   - Disallow GlobalCaps and ResultCaps in such arguments.
     *   - If a corresponding formal type parameter is declared or implied @use,
     *     charge the deep capture set of the argument to the environent.
     *  @param  fn   the type application, of type TypeApply or TypeTree
     *  @param  sym  the constructor symbol (could be a method or a val or a class)
     *  @param  args the type arguments
     */
    def markFreeTypeArgs(fn: Tree, sym: Symbol, args: List[Tree])(using Context): Unit =
      def isExempt = sym.isTypeTestOrCast || defn.capsErasedValueMethods.contains(sym)
      if !isExempt then
        val paramNames = atPhase(thisPhase.prev):
          fn.tpe.widenDealias match
            case tl: TypeLambda => tl.paramNames
            case ref: AppliedType if ref.typeSymbol.isClass => ref.typeSymbol.typeParams.map(_.name)
            case t => args.map(_ => EmptyTypeName)

        for case (arg: TypeTree, pname) <- args.lazyZip(paramNames) do
          def where = if sym.exists then i" in an argument of $sym" else ""
          def addendum =
            if arg.isInferred
            then i"\nThis is often caused by a local capability$where\nleaking as part of its result."
            else ""
          def errTree = if !arg.isInferred && arg.span.exists then arg else fn
          disallowBadRootsIn(arg.nuType, NoSymbol,
            i"Type variable $pname of $sym", "be instantiated to", addendum, errTree.srcPos)

          val param = fn.symbol.paramNamed(pname)
          if param.isUseParam then markFree(arg.nuType.deepCaptureSet, errTree)
    end markFreeTypeArgs

// ---- Check for leakages of reach capabilities ------------------------------

    /** If capability `c` refers to a parameter that is not implicitly or explicitly
     *  @use declared, report an error.
     */
    def checkUseDeclared(c: Capability, pos: SrcPos)(using Context): Unit =
      c.paramPathRoot match
        case ref: NamedType if !ref.symbol.isUseParam =>
          val what = if ref.isType then "Capture set parameter" else "Local reach capability"
          def mitigation =
            if ccConfig.allowUse
            then i"\nTo allow this, the ${ref.symbol} should be declared with a @use annotation."
            else if !ref.isType then i"\nYou could try to abstract the capabilities referred to by $c in a capset variable."
            else ""
          report.error(
            em"$what $c leaks into capture scope${ref.symbol.owner.qualString("of")}.$mitigation",
            pos)
        case _ =>

    /** Warn if there is an unboxed reach capability in the result of a method
     *  that refers to a parameter. These uses will almost always lead to checkUseDeclared
     *  failures later on.
     */
    def checkNoUnboxedReaches(tree: DefDef)(using Context): Unit = tree.tpt match
      case tpt: TypeTree if !tpt.isInferred =>
        def checkType(tp: Type) = tp match
          case tp @ CapturingType(_, refs) =>
            if !tp.isBoxed then
              for ref <- refs.elems do
                if ref.isReach then
                  ref.paramPathRoot match
                    case tp: TermRef =>
                      report.warning(
                        em"""Reach capability `${ref.showAsCapability}` in function result refers to ${tp.symbol}.
                            |To avoid errors of the form "Local reach capability $ref leaks into capture scope ..."
                            |you should replace the reach capability with a new capset variable in ${tree.symbol}.""",
                        tree.tpt.srcPos)
                    case _ =>
          case _ =>
        tpt.nuType.foreachPart(checkType, StopAt.Static)
      case _ =>

// ---- Rechecking operations for different kinds of trees----------------------

    /** If `tp` (possibly after widening singletons) is an ExprType
     *  of a parameterless method, map ResultCap instances in it to LocalCap instances
     */
    def mapResultRoots(tp: Type, sym: Symbol)(using Context): Type =
      tp.widenSingleton match
        case tp: ExprType if sym.is(Method) =>
          resultToAny(tp, Origin.ResultInstance(tp, sym))
        case _ =>
          tp

    /** Rechecking idents involves:
     *   - adding call captures for idents referring to methods
     *   - marking as free the identifier with any selections or .rd
     *     modifiers implied by the expected type
     */
    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      val sym = tree.symbol
      if sym.isOneOf(MethodOrLazy) then
        // If ident refers to a parameterless method or lazy val, charge its cv to the environment.
        // Lazy vals are like parameterless methods: accessing them may trigger initialization
        // that uses captured references.
        includeCallCaptures(sym, sym.info, tree)
      else
        if sym.isMutableVar && sym.owner.isTerm && pt != LhsProto then
          // When we have `var x: A^{c} = ...` where `x` is a local variable then
          // when dereferencing `x` we also need to charge `c`.
          // For fields it's not a problem since `c` would already have been
          // charged for the prefix `p` in `p.x`.
          markFree(sym.info.captureSet, tree)
        if sym.exists then
          markPathFree(sym.termRef, pt, tree)
      mapResultRoots(super.recheckIdent(tree, pt), tree.symbol)

    override def recheckThis(tree: This, pt: Type)(using Context): Type =
      markPathFree(tree.tpe.asInstanceOf[ThisType], pt, tree)
      super.recheckThis(tree, pt)

    /** Add all selections and also any `.rd modifier implied by the expected
     *  type `pt` to `ref`. Expand the marked tree accordingly to take account of
     *  the added path. Example:
     *  If we have `x` and the expected type says we select that with `.a.b`
     *  where `b` is a read-only method, we charge `x.a.rd` for tree `x.a.b`
     *  instead of just charging `x`.
     */
    private def markPathFree(ref: TermRef | ThisType, pt: Type, tree: Tree)(using Context): Unit = pt match
      case pt: PathSelectionProto
      if ref.isTracked && !pt.selector.isOneOf(MethodOrLazyOrMutable) =>
        // if `ref` is not tracked then the selection could not give anything new
        // class SerializationProxy in stdlib-cc/../LazyListIterable.scala has an example where this matters.
        val sel = ref.select(pt.selector).asInstanceOf[TermRef]
        markPathFree(sel, pt.pt, pt.select)
      case _ =>
        markFree(ref.mapLocalMutable.adjustReadOnly(pt), tree)

    /** The expected type for the qualifier of a selection. If the selection
     *  could be part of a capability path or is a a read-only method, we return
     *  a PathSelectionProto.
     */
    override def selectionProto(tree: Select, pt: Type)(using Context): Type =
      if tree.symbol.is(Package) then super.selectionProto(tree, pt)
      else PathSelectionProto(tree, pt)

    /** A specialized implementation of the selection rule.
     *
     *  E |- f: T{ m: R^Cr }^{f}
     *  ------------------------
     *  E |- f.m: R^C
     *
     *  The implementation picks as `C` one of `{f}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f}` if it might subcapture Cr
     *  and picks Cr otherwise.
     */
    override def recheckSelection(tree: Select, qualType: Type, name: Name, pt: Type)(using Context) = {
      def disambiguate(denot: Denotation): Denotation = denot match
        case MultiDenotation(denot1, denot2) =>
          // This case can arise when we try to merge multiple types that have different
          // capture sets on some part. For instance an asSeenFrom might produce
          // a bi-mapped capture set arising from a substition. Applying the same substitution
          // to the same type twice will nevertheless produce different capture sets which can
          // lead to a failure in disambiguation since neither alternative is better than the
          // other in a frozen constraint. An example test case is disambiguate-select.scala.
          // We address the problem by disambiguating while ignoring all capture sets as a fallback.
          withMode(Mode.IgnoreCaptures) {
            disambiguate(denot1).meet(disambiguate(denot2), qualType)
          }
        case _ => denot

      // Don't allow update methods to be called unless the qualifier captures
      // an exclusive reference.
      if tree.symbol.isUpdateMethod then
        checkUpdate(qualType, tree.srcPos):
          i"Cannot call update ${tree.symbol} of ${qualType.showRef}"

      val origSelType = recheckSelection(tree, qualType, name, disambiguate)
      val selType = mapResultRoots(origSelType, tree.symbol)
      val selWiden = selType.widen

      def capturesResult = origSelType.widenSingleton match
        case ExprType(resType) => resType.captureSet.containsResultCapability
        case _ => false

      // Don't apply the rule
      //   - on the LHS of assignments, or
      //   - if the qualifier or selection type is boxed, or
      //   - the selection is either a trackable capture reference or a pure type, or
      //   - if the selection is of a parameterless method capturing a ResultCap
      if noWiden(selType, pt)
          || qualType.isBoxedCapturing
          || selType.isBoxedCapturing
          || selWiden.isBoxedCapturing
          || selType.isTrackableRef
          || selWiden.captureSet.isAlwaysEmpty
          || capturesResult
      then
        selType
      else
        val qualCs = qualType.captureSet
        val selCs = selType.captureSet
        capt.println(i"pick one of $qualType, ${selType.widen}, $qualCs, $selCs ${selWiden.captureSet} in $tree")

        if qualCs.mightSubcapture(selCs)
            && !pt.stripCapturing.isInstanceOf[SingletonType]
        then
          selWiden.stripCapturing.capturing(qualCs)
            .showing(i"alternate type for select $tree: $selType --> $result, $qualCs / $selCs", capt)
        else
          selType
    }//.showing(i"recheck sel $tree, $qualType = $result")

    /** Recheck `caps.unsafe.unsafeAssumePure(...)` */
    def applyAssumePure(tree: Apply, pt: Type)(using Context): Type =
      val arg :: Nil = tree.args: @unchecked
      val argType0 = recheck(arg, pt.stripCapturing.capturing(LocalCap(Origin.UnsafeAssumePure)))
      val argType =
        if argType0.captureSet.isAlwaysEmpty then argType0
        else argType0.widen.stripCapturing
      capt.println(i"rechecking unsafeAssumePure of $arg with $pt: $argType")
      super.recheckFinish(argType, tree, pt)

    /** Recheck applications, with special handling of unsafeAssumePure,
     *  unsafeDiscardUses, and freeze.
     *  More work is done in `recheckApplication`, `recheckArg` and `instantiate` below.
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val meth = tree.fun.symbol
      if meth == defn.Caps_unsafeAssumePure then
        applyAssumePure(tree, pt)
      else if meth == defn.Caps_unsafeDiscardUses then
        val arg :: Nil = tree.args: @unchecked
        withDiscardedUses(recheck(arg, pt))
      else if meth == defn.Caps_freeze then
        freeze(super.recheckApply(tree, pt), tree.srcPos)
      else
        val res = super.recheckApply(tree, pt)
        includeCallCaptures(meth, res, tree)
        res

    /** Recheck argument against an instantiated version of `formal` where toplevel `any`
     *  occurrences are replaced by LocalCap instances. Also, if formal parameter carries a `@use`
     *  or @consume, charge the deep capture set of the actual argument to the environment.
     *  TODO: Maybe not charge deep capture sets for consume?
     */
    protected override def recheckArg(arg: Tree, formal: Type, pref: ParamRef, app: Apply)(using Context): Type =
      val instantiatedFormal = globalCapToLocal(formal, Origin.Formal(pref, app))
      val argType = recheck(arg, instantiatedFormal)
        .showing(i"recheck arg $arg vs $instantiatedFormal = $result", capt)
      if formal.hasAnnotation(defn.UseAnnot) || formal.hasAnnotation(defn.ConsumeAnnot) then
        // The @use and/or @consume annotation is added to `formal` when creating methods types.
        // See [[MethodTypeCompanion.adaptParamInfo]].
        capt.println(i"charging deep capture set of $arg: ${argType} = ${argType.deepCaptureSet}")
        markFree(argType.deepCaptureSet, arg)
      if formal.containsGlobalAny then
        sepCheckFormals(arg) = instantiatedFormal
      argType

    /** Map existential captures in result to a new local `any` and implement the
     *  following rule:
     *
     *  E |- q: Tq^Cq
     *  E |- q.f: Ta^Ca ->Cf Tr^Cr
     *  E |- a: Ta
     *  ---------------------
     *  E |- f(a): Tr^C
     *
     *  If the function `f` does not have an `@use` parameter, then
     *  any unboxing it does would be charged to the environment of the function
     *  so they have to appear in Cq. Since any capabilities of the result of the
     *  application must already be present in the application, an upper
     *  approximation of the result capture set is Cq \union Ca, where `Ca`
     *  is the capture set of the argument.
     *  If the function `f` does have an `@use` parameter, then it could in addition
     *  unbox reach capabilities over its formal parameter. Therefore, the approximation
     *  would be `Cq \union dcs(Ta)` instead.
     *  If the approximation might subcapture the declared result Cr, we pick it for C
     *  otherwise we pick Cr.
     */
    protected override
    def recheckApplication(tree: Apply, qualType: Type, funType: MethodType, argTypes: List[Type])(using Context): Type =
      val resultType = super.recheckApplication(tree, qualType, funType, argTypes)
      val appType = resultToAny(resultType, Origin.ResultInstance(funType, tree.symbol))
      val qualCaptures = qualType.captureSet
      val argCaptures =
        for (argType, formal) <- argTypes.lazyZip(funType.paramInfos) yield
          if formal.hasAnnotation(defn.UseAnnot) then argType.deepCaptureSet else argType.captureSet
      appType match
        case appType @ CapturingType(appType1, refs)
        if qualType.exists
            && !qualType.isBoxedCapturing
            && !resultType.isBoxedCapturing
            && !tree.fun.symbol.isConstructor
            && !resultType.captureSet.containsResultCapability
            && !resultType.captureSet.elems.exists(_.derivesFromUnscoped)
            && qualCaptures.mightSubcapture(refs)
            && argCaptures.forall(_.mightSubcapture(refs)) =>
          val callCaptures = argCaptures.foldLeft(qualCaptures)(_ ++ _)
          appType.derivedCapturingType(appType1, callCaptures)
            .showing(i"narrow $tree: $appType, refs = $refs, qual-cs = ${qualType.captureSet} = $result", capt)
        case appType =>
          appType

    private def isDistinct(xs: List[Type]): Boolean = xs match
      case x :: xs1 => xs1.isEmpty || !xs1.contains(x) && isDistinct(xs1)
      case Nil => true

    /** Handle an application of method `sym` with type `mt` to arguments of types `argTypes`.
     *  This means
     *   - Instantiate result type with actual arguments
     *   - if `sym` is a constructor, refine its type with `refineInstanceType`
     */
    override def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val ownType =
        if !mt.isResultDependent then mt.resType
        else SubstParamsMap(mt, argTypes)(mt.resType)
      if sym.isConstructor then refineConstructorInstance(ownType, mt, argTypes, sym)
      else ownType

    /** Refine the type returned from a constructor as follows:
     *   - remember types of arguments corresponding to tracked parameters in refinements.
     *   - add capture set of instantiated class and capture set of constructor to capture set of result type.
     *  Note: This scheme does not distinguish whether a capture is made by the constructor
     *  only or by a method in the class. Both captures go into the result type. We
     *  could be more precise by distinguishing the two capture sets.
     */
    private def refineConstructorInstance(resType: Type, mt: MethodType, argTypes: List[Type], constr: Symbol)(using Context): Type =
      val cls = constr.owner.asClass

      /** First half of result pair:
       *  Refine the type of a constructor call `new C(t_1, ..., t_n)`
       *  to C{val x_1: @refineOverride T_1, ..., x_m: @refineOverride T_m}
       *  where x_1, ..., x_m are the tracked parameters of C and
       *  T_1, ..., T_m are the types of the corresponding arguments. The @refineOveride
       *  annotations avoid problematic intersections of capture sets when those
       *  parameters are selected.
       *
       *  Second half: union of initial capture set, all capture sets of arguments
       *  to tracked parameters, and the capture set implied by the fields of the class.
       */
      def addParamArgRefinements(core: Type, initCs: CaptureSet): (Type, CaptureSet) =
        var refined: Type = core
        var allCaptures: CaptureSet = initCs ++ captureSetImpliedByFields(cls, core)
        for (getterName, argType) <- mt.paramNames.lazyZip(argTypes) do
          val getter = cls.info.member(getterName).suchThat(_.isRefiningParamAccessor).symbol
          if !getter.is(Private) && getter.hasTrackedParts then
            refined = refined.refinedOverride(getterName, argType.unboxed) // Yichen you might want to check this
            if getter.hasAnnotation(defn.ConsumeAnnot) then
              () // We make sure in checkClassDef, point (6), that consume parameters don't
                 // contribute to the class capture set
            else allCaptures ++= argType.captureSet
        (refined, allCaptures)

      /** Augment result type of constructor with refinements and captures.
       *  @param  core   The result type of the constructor
       *  @param  initCs The initial capture set to add, not yet counting capture sets from arguments
       */
      def augmentConstructorType(core: Type, initCs: CaptureSet): Type = core match
        case core: MethodType =>
          // more parameters to follow; augment result type
          core.derivedLambdaType(resType = augmentConstructorType(core.resType, initCs))
        case CapturingType(parent, refs) =>
          // can happen for curried constructors if instantiate of a previous step
          // added capture set to result.
          augmentConstructorType(parent, initCs ++ refs)
        case _ =>
          val (refined, cs) = addParamArgRefinements(core, initCs)
          refined.capturing(cs)

      augmentConstructorType(resType, mapClassCaptures(cls, resType, capturedVars(cls)))
        .showing(i"constr type $mt with $argTypes%, % in $constr = $result", capt)
    end refineConstructorInstance

    /** Map locals with an as-seen-from relative to the prefix path of the created class
     *  if the prefix is non-trivial,
     */
    def mapClassCaptures(cls: ClassSymbol, core: Type, locals: CaptureSet)(using Context): CaptureSet =
      if cls.isStatic || cls.owner.isTerm then locals
      else core match
        case core: MethodType => mapClassCaptures(cls, core.resType, locals)
        case _ =>
          core.underlyingClassRef(refinementOK = true) match
            case TypeRef(prefix: ThisType, _) if prefix.cls == cls => locals
            case TypeRef(prefix, _) => locals.map(AsSeenFromMap(prefix, cls.owner))
            case _ => locals

    private def memberCaps(mbr: Symbol)(using Context): List[Capability] =
      if contributesLocalCapToClass(mbr) then
        mbr.info.spanCaptureSet.elems
          .filter(_.isTerminalCapability)
          .toList
      else Nil

    /** If `mbr` is a field that has (possibly restricted) LocalCaps in its span capture set,
     *  their classifiers, otherwise the empty list.
     */
    private def classifiersOfLocalCapsInType(mbr: Symbol)(using Context): List[ClassSymbol] =
      memberCaps(mbr).map(_.classifier).collect:
        case cl: ClassSymbol => cl

    private def allLocalCapsInTypeAreRO(mbr: Symbol)(using Context): Boolean =
      memberCaps(mbr).forall(_.isReadOnly)

    /** The additional capture set implied by the capture sets of its fields. This
     *  is either empty or, if some fields have a terminal capability in their span
     *  capture sets, it consists of a single LocalCap that subsumes all these terminal
     *  capabilities. Class parameters are not counted. If the type extends Separate,
     *  we add a LocalCap in any case -- this is because we can currently hide
     *  mutability in array vals if separation checking is off, an example is
     *  neg-customargs/captures/matrix.scala.
     */
    def captureSetImpliedByFields(cls: ClassSymbol, core: Type)(using Context): CaptureSet =
      var infos: List[String] = Nil
      def pushInfo(msg: => String) =
        if ctx.settings.YccVerbose.value then infos = msg :: infos

      def knownFields(cls: ClassSymbol) =
        setup.fieldsWithExplicitTypes             // pick fields with explicit types for classes in this compilation unit
          .getOrElse(cls, cls.info.decls.toList)  // pick all symbols in class scope for other classes

      /** The classifiers of the LocalCaps in the span capture sets of all fields
       *  in the given class `cls`.
       */
      def impliedClassifiers(cls: Symbol): List[ClassSymbol] = cls match
        case cls: ClassSymbol =>
          var fieldClassifiers = knownFields(cls).flatMap(classifiersOfLocalCapsInType)
          val parentClassifiers =
            cls.parentSyms.map(impliedClassifiers).filter(_.nonEmpty)
          if fieldClassifiers.isEmpty && parentClassifiers.isEmpty
          then Nil
          else parentClassifiers.foldLeft(fieldClassifiers.distinct)(dominators)
        case _ => Nil

      def impliedReadOnly(cls: Symbol): Boolean = cls match
        case cls: ClassSymbol =>
          val fieldsRO = knownFields(cls).forall(allLocalCapsInTypeAreRO)
          val parentsRO = cls.parentSyms.forall(impliedReadOnly)
          fieldsRO && parentsRO
        case _ =>
          false

      def maybeRO(ref: Capability) =
        if !cls.isSeparate && impliedReadOnly(cls) then ref.readOnly else ref

      def localCap = // TODO Maybe drop this?
        LocalCap(Origin.NewInstance(core)).tap: localCap =>
          if ctx.settings.YccVerbose.value then
            pushInfo(i"Note: instance of $cls captures an $localCap that comes from a field")
            report.echo(infos.mkString("\n"), ctx.owner.srcPos)

      var implied = impliedClassifiers(cls)
      if cls.isSeparate then implied = dominators(cls.classifier :: Nil, implied)
      knownLocalCapClassifiers.getOrElseUpdate(cls, implied) match
        case Nil => CaptureSet.empty
        case cl :: Nil =>
          val result = localCap
          result.hiddenSet.adoptClassifier(cl)
          maybeRO(result).singletonCaptureSet
        case _ => maybeRO(localCap).singletonCaptureSet
    end captureSetImpliedByFields

    /** Recheck type applications:
     *   - Map existential captures in result to new local `any`s
     *   - include captures of called methods in environment
     *   - don't allow `any` to appear covariantly in type arguments
     *   - special handling of `contains[A, B]` calls
     */
    override def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      val meth = tree.fun match
        case fun @ Select(qual, nme.apply) => qual.symbol.orElse(fun.symbol)
        case fun => fun.symbol
      def methDescr = if meth.exists then i"$meth's type " else ""
      markFreeTypeArgs(tree.fun, meth, tree.args)
      val funType = super.recheckTypeApply(tree, pt)
      val res = resultToAny(funType, Origin.ResultInstance(funType, meth))
      includeCallCaptures(tree.symbol, res, tree)
      checkContains(tree)
      res
    end recheckTypeApply

    /** Faced with a tree of form `caps.contansImpl[CS, r.type]`, check that `R` is a tracked
     *  capability and assert that `{r} <: CS`.
     */
    def checkContains(tree: TypeApply)(using Context): Unit = tree match
      case ContainsImpl(csArg, refArg) =>
        val cs = csArg.nuType.captureSet
        val ref = refArg.nuType
        capt.println(i"check contains $cs , $ref")
        ref.stripCapturing match
          case ref: Capability if ref.isTracked =>
            checkElem(ref, cs, tree.srcPos)
          case _ =>
            report.error(em"$refArg is not a tracked capability", refArg.srcPos)
      case _ =>

    override def recheckAssign(tree: Assign)(using Context): Type =
      val lhsType = recheck(tree.lhs, LhsProto)
      recheck(tree.rhs, lhsType.widen)
      lhsType match
        case lhsType @ TermRef(qualType, _)
        if !lhsType.symbol.hasAnnotation(defn.UntrackedCapturesAnnot) =>
          checkUpdate(qualType, tree.srcPos)(i"Cannot assign to field ${lhsType.name} of ${qualType.showRef}")
        case _ =>
      defn.UnitType

    /** Recheck Closure node: add the captured vars of the anonymoys function
     *  to the result type. See also `recheckClosureBlock` which rechecks the
     *  block containing the anonymous function and the Closure node.
     */
    override def recheckClosure(tree: Closure, pt: Type, forceDependent: Boolean)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      capt.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt, forceDependent).capturing(cs)
        .showing(i"rechecked closure $tree / $pt = $result", capt)

    /** Recheck a lambda of the form
     *      { def $anonfun(...) = ...; closure($anonfun, ...)}
     */
    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
      val anonfun = mdef.symbol

      /** Does `tp` contain capturing types with unsolved capture set variables? */
      def hasCapsetVars(tp: Type) =
        def isVarCapturing(tp: Type) = tp match
          case CapturingType(_, refs) => !refs.isConst
          case _ => false
        tp.existsPart(isVarCapturing, stopAt = StopAt.Package)

      /** Use the capture sets of `tp2` in matching capture set variables in `tp1`.
       *  This is done by copying the elements from `tp2` sets to `tp1` sets
       *  and marking the `tp1` sets as solved.
       */
      def adoptCaptures(tp1: Type, tp2: Type)(using Context): Unit = (tp1.dealias, tp2.dealias) match
        case (CapturingType(parent1, refs1), CapturingType(parent2, refs2)) =>
          if !refs1.isConst && refs2.isConst && refs1.elems.forall(refs2.elems.contains) then
            refs1.asVar.elems = refs2.elems
            refs1.asVar.markSolved(provisional = false)
          adoptCaptures(parent1, parent2)
        case (CapturingType(parent1, refs1), tp2) =>
          if !refs1.isConst && refs1.elems.isEmpty then
            refs1.asVar.markIgnored()
          adoptCaptures(parent1, tp2)
        case (tp1, CapturingType(parent2, _)) =>
          adoptCaptures(tp1, parent2)
        case (FunctionOrMethod(args1, res1), FunctionOrMethod(args2, res2)) =>
          args1.lazyZip(args2).foreach(adoptCaptures)
          adoptCaptures(res1, res2)
        case (AppliedType(fn1, args1), AppliedType(fn2, args2)) =>
          adoptCaptures(fn1, fn2)
          args1.lazyZip(args2).foreach(adoptCaptures)
        case (RefinedType(parent1, _, rinfo1), RefinedType(parent2, _, rinfo2)) =>
          adoptCaptures(parent1, parent2)
          adoptCaptures(rinfo1, rinfo2)
        case _ =>

      def updateResult(tp: Type) = mdef.tpt.updNuType(tp)

      /** Propagate what we know of parameters and results into the closure.
       *  This improves error messages and avoids shortcomings of inference
       *  which cannot infer new quantifiers (i24901.scala is an example).
       */
      def matchParamsAndResult(paramss: List[ParamClause], pt: Type): Unit = paramss match
        case params :: paramss1 => pt.dealias match
          case CapturingType(parent, _) =>
            matchParamsAndResult(paramss, parent)
          case defn.PolyFunctionOf(poly: PolyType) =>
            assert(params.hasSameLengthAs(poly.paramInfos))
            matchParamsAndResult(paramss1, poly.instantiate(params.map(_.symbol.typeRef)))
          case FunctionOrMethod(argTypes, resType) =>
            assert(params.hasSameLengthAs(argTypes), i"$mdef vs $pt, ${params}")
            inContext(ctx.withOwner(anonfun)) {
              // Propagate argument types to parameter types with inferred types
              for (argType, param) <- argTypes.lazyZip(params) do
                param.asInstanceOf[ValDef].tpt match
                  case paramTpt: InferredTypeTree =>
                    val localArgType = globalCapToLocal(argType, Origin.Parameter(param.symbol))
                    adoptCaptures(param.symbol.info, localArgType)
                  case _ =>

              // Propagate fully defined result types
              if resType.isValueType && !hasCapsetVars(resType) then
                // Try to update the declared result type of the closure `mdef.tpt` with the expected
                // result type, in order to propagate constraints into the closure.
                // Note: We could use adoptCaptures instead, but this seems to give somewhat worse
                // error messages.
                pt match
                  case RefinedType(_, _, mt: MethodType) =>
                    if !mt.isResultDependent then
                      // If mt is result dependent we could compensate this by
                      // internalizing `resType.substParams(mt, params.tpes)`.
                      // But this tends to give worse error messages, so we refrain
                      // from doing that and don't update the local result type instead.
                      updateResult(Internalize(mt)(resType))
                  case _ =>
                    updateResult(resType)
            }
          case SAMType(mt1, _) =>
            matchParamsAndResult(paramss, mt1.derivedLambdaType(resType = WildcardType))
              // We get failures in stdlib's JavaCollectionWrappers.scala when we
              // match result types against results of SAM methods. Not clear where
              // they come from. We work aorund this by not passing down the result
              // type of a SAM method.
          case _ =>
        case Nil =>

      openClosures = (anonfun, pt) :: openClosures
        // openClosures is needed for errors but currently makes no difference
        // TODO follow up on this
      try
        capt.println(i"recheck closure block $mdef: ${anonfun.infoOrCompleter}")
        if !anonfun.isCompleted then
          // anonfun is already completed if its parameter and result types do not change in cc.
          matchParamsAndResult(mdef.paramss, pt)
          anonfun.ensureCompleted() // this will recheck def
        else
          recheckDef(mdef, anonfun)
        recheckClosure(expr, pt, forceDependent = true)
      finally
        openClosures = openClosures.tail
    end recheckClosureBlock

    /** Add var mirrors to the list of block-local symbols to avoid */
    override def avoidLocals(tp: Type, symsToAvoid: => List[Symbol])(using Context): Type =
      val locals = symsToAvoid
      val varMirrors = locals.collect:
        case local if local.termRef.isLocalMutable => local.varMirror
      super.avoidLocals(tp, varMirrors ++ locals)

    /** Elements of a SeqLiteral instantiate a Seq or Array parameter, so they
     *  should be boxed.
     */
    override def seqLiteralElemProto(tree: SeqLiteral, pt: Type, declared: Type)(using Context) =
      super.seqLiteralElemProto(tree, pt, declared).boxed

    /** Recheck val and var definitions:
     *   - disallow `any` in the type of mutable vars.
     *   - for externally visible definitions: check that their inferred type
     *     does not refine what was known before capture checking.
     *   - Interpolate contravariant capture set variables in result type.
     *   - for lazy vals: create a nested environment to track captures (similar to methods)
     */
    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      val savedEnv = curEnv
      val runInConstructor = !sym.isOneOf(Param | ParamAccessor | Lazy | NonMember)
      try
        if sym.is(Mutable) then
          if !sym.hasAnnotation(defn.UncheckedCapturesAnnot) then
            val addendum = setup.capturedBy.get(sym) match
              case Some(encl) =>
                val enclStr =
                  if encl.isAnonymousFunction then
                    val location = setup.anonFunCallee.get(encl) match
                      case Some(meth) if meth.exists => i" argument in a call to $meth"
                      case _ => ""
                    s"an anonymous function$location"
                  else encl.show
                i"\n\nNote that $sym does not count as local since it is captured by $enclStr"
              case _ =>
                ""
            disallowBadRootsIn(
              tree.tpt.nuType, NoSymbol, i"Mutable $sym", "have type", addendum, sym.srcPos)
          if ccConfig.strictMutability
              && sym.owner.isClass
              && !sym.owner.derivesFrom(defn.Caps_Stateful)
              && !sym.hasAnnotation(defn.UntrackedCapturesAnnot) then
            report.error(
              em"""Mutable $sym is defined in a class that does not extend `Stateful`.
                  |The variable needs to be annotated with `untrackedCaptures` to allow this.""",
              tree.namePos)

        // Lazy vals need their own environment to track captures from their RHS,
        // similar to how methods work
        if sym.is(Lazy) then
          val localSet = capturedVars(sym)
          if localSet ne CaptureSet.empty then
            curEnv = Env(sym, EnvKind.Regular, localSet, curEnv, nestedClosure = NoSymbol)
        else if runInConstructor then
          pushConstructorEnv()

        checkInferredResult(super.recheckValDef(tree, sym), tree)
      finally
        if !sym.is(Param) then
          // Parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateIfInferred(tree.tpt, sym)

        def declaredCaptures = tree.tpt.nuType.captureSet
        curEnv = savedEnv

        if runInConstructor && savedEnv.owner.isClass then
          markFree(declaredCaptures, tree, addUseInfo = false)
    end recheckValDef

    /** Recheck method definitions:
     *   - check body in a nested environment that tracks uses, in  a nested level,
     *     and in a nested context that knows abaout Contains parameters so that we
     *     can assume they are true.
     *   - for externally visible definitions: check that their inferred type
     *     does not refine what was known before capture checking.
     *   - Interpolate contravariant capture set variables in result type unless
     *     def is anonymous.
     */
    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      if Synthetics.isExcluded(sym) then sym.info
      else {
        // Under the deferredReaches setting: If rhs ends in a closure or
        // anonymous class, the corresponding symbol
        def nestedClosure(rhs: Tree)(using Context): Symbol =
          if !ccConfig.deferredReaches then NoSymbol
          else rhs match
            case Closure(_, meth, _) => meth.symbol
            case Apply(fn, _) if fn.symbol.isConstructor && fn.symbol.owner.isAnonymousClass => fn.symbol.owner
            case Block(_, expr) => nestedClosure(expr)
            case Inlined(_, _, expansion) => nestedClosure(expansion)
            case Typed(expr, _) => nestedClosure(expr)
            case _ => NoSymbol

        val saved = curEnv
        val localSet = capturedVars(sym)
        if localSet ne CaptureSet.empty then
          curEnv = Env(sym, EnvKind.Regular, localSet, curEnv, nestedClosure(tree.rhs))

        // ctx with AssumedContains entries for each Contains parameter
        val bodyCtx =
          var ac = CaptureSet.assumedContains
          for paramSyms <- sym.paramSymss do
            for case ContainsParam(cs, ref) <- paramSyms do
              ac = ac.updated(cs, ac.getOrElse(cs, SimpleIdentitySet.empty) + ref)
          if ac.isEmpty then ctx
          else ctx.withProperty(CaptureSet.AssumedContains, Some(ac))

        checkNoUnboxedReaches(tree)

        try checkInferredResult(super.recheckDefDef(tree, sym)(using bodyCtx), tree)
        finally
          if !sym.isAnonymousFunction then
            // Anonymous functions propagate their type to the enclosing environment
            // so it is not in general sound to interpolate their types.
            interpolateIfInferred(tree.tpt, sym)
          curEnv = saved
      }

    /** Is symbol exempt from checking that its type or uses clause must
     *  be given explicitly? This is the case for symbols that are not
     *  visible outside the compilation unit where they are defined,
     *  and also for two pragmatic exemptions, explained below.
     */
    def isExemptFromExplicitChecks(sym: Symbol)(using Context): Boolean =
      sym.isLocalToCompilationUnit
      || ctx.owner.enclosingPackageClass.isEmptyPackage
        // We make an exception for symbols in the empty package.
        // these could theoretically be accessed from other files in the empty package, but
        // usually it would be too annoying to require explicit types.
      || sym.name.is(DefaultGetterName)
        // Default getters are exempted since otherwise it would be
        // too annoying. This is a hole since a defualt getter's result type
        // might leak into a type variable.

    /** Two tests for member definitions with inferred types:
     *
     *   1. If val or def definition with inferred (result) type is visible
     *      in other compilation units, check that the actual inferred type
     *      conforms to the expected type where all inferred capture sets are dropped.
     *      This ensures that if files compile separately, they will also compile
     *      in a joint compilation.
     *   2. If a val has an inferred type with a terminal capability in its span capset,
     *      check that it this capability is subsumed by the capset that was inferred
     *      for the class from its other fields via `captureSetImpliedByFields`.
     *      That capset is defined to take into account all fields but is computed
     *      only from fields with explicitly given types in order to avoid cycles.
     *      See comment on Setup.fieldsWithExplicitTypes. So we have to make sure
     *      that fields with inferred types would not change that capset.
     */
    def checkInferredResult(tp: Type, tree: ValOrDefDef)(using Context): Type =
      val sym = tree.symbol

      def fail(tree: Tree, expected: Type, notes: List[Note]): Unit =
        def maybeResult = if sym.is(Method) then " result" else ""
        report.error(
          em"""$sym needs an explicit$maybeResult type because the inferred type does not conform to
              |the type that is externally visible in other compilation units.
              |
              | Inferred type          : ${tree.tpe}
              | Externally visible type: $expected""",
          tree.srcPos)

      def addendum(expected: Type) = Note:
        def result = if tree.isInstanceOf[ValDef] then"" else " result"
        i"""
          |
          |Note that the expected type $expected
          |is the previously inferred$result type of $sym
          |which is also the type seen in separately compiled sources.
          |The new inferred type $tp
          |must conform to this type."""

      def covers(classCapset: CaptureSet, fieldClassifiers: List[ClassSymbol]): Boolean =
        fieldClassifiers.forall: cls =>
          classCapset.elems.exists:
            case localCap: LocalCap => cls.isSubClass(localCap.hiddenSet.classifier)
            case _ => false

      tree.tpt match
        case tpt: InferredTypeTree =>
          // Test point (1) of doc comment above
          if !isExemptFromExplicitChecks(sym) then // Symbols that can't be seen outside the compilation unit can have inferred types
            val expected = tpt.tpe.dropAllRetains
            todoAtPostCheck += { () =>
              withGlobalCapAsRoot:
                testAdapted(tp, expected, tree.rhs, addendum(expected) :: Nil)(fail)
                // The check that inferred <: expected is done after recheck so that it
                // does not interfere with normal rechecking by constraining capture set variables.
            }
          // Test point (2) of doc comment above
          if sym.owner.isClass
              && contributesLocalCapToClass(sym)
              && !CaptureSet.isAssumedPure(sym)
          then
            todoAtPostCheck += { () =>
              val cls = sym.owner.asClass
              val fieldClassifiers = classifiersOfLocalCapsInType(sym)
              val classCapset = captureSetImpliedByFields(cls, cls.appliedRef)
              if !covers(classCapset, fieldClassifiers) then
                report.error(
                  em"""$sym needs an explicit type because it captures a root capability in its type ${tree.tpt.nuType}.
                      |Fields capturing a root capability need to be given an explicit type unless the capability is already
                      |subsumed by the computed capability of the enclosing class.""",
                tpt.srcPos)
            }
        case _ =>
      tp
    end checkInferredResult

    /** The normal rechecking if `sym` was already completed before */
    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    /** Check a ValDef or DefDef as an action performed in a completer. Since
     *  these checks can appear out of order, we need to first create the correct
     *  environment for checking the definition.
     */
    def completeDef(tree: ValOrDefDef, sym: Symbol, completer: LazyType)(using Context): Type =
      val saved = curEnv
      try
        // Setup environment to reflect the new owner.
        val envForOwner: Map[Symbol, Env] = curEnv.outersIterator
          .map(e => (e.owner, e))
          .toMap
        def restoreEnvFor(sym: Symbol): Env =
          if definesEnv(sym) then
            val localSet = capturedVars(sym)
            if localSet eq CaptureSet.empty then rootEnv
            else envForOwner.get(sym) match
              case Some(e) => e
              case None => Env(sym, EnvKind.Regular, localSet, restoreEnvFor(sym.owner))
          else restoreEnvFor(sym.owner)
        curEnv = restoreEnvFor(sym.owner)
        capt.println(i"Complete $sym in ${curEnv.outersIterator.toList.map(_.owner)}")
        try recheckDef(tree, sym)
        finally completed += sym
      finally
        curEnv = saved

    override def recheckTypeDef(tree: TypeDef, sym: Symbol)(using Context): Type =
      try super.recheckTypeDef(tree, sym)
      finally completed += sym

    /** Recheck classDef by enforcing the following class-specific capture set relations:
     *   1. The capture set of a class includes the capture sets of its parents.
     *   2. The capture set of the self type of a class includes the capture set of the class.
     *   3. The capture set of the self type of a class includes the capture set of every class
     *      parameter, unless the parameter is marked @constructorOnly or @untrackedCaptures.
     *   4. If the class extends a pure base class, the capture set of the self type must be empty.
     *   5. Check that trait parents represented as applied types don't have `any` in their
     *      type arguments. Charge deep capture sets of type arguments to non-reserved typevars
     *      to the environment. Other generic parents are represented as TypeApplys, where the
     *      same check is already done in the TypeApply.
     */
    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      if Feature.enabled(Feature.separationChecking) then sepChecksEnabled = true
      val localSet = capturedVars(cls)

      // (1) Capture set of a class includes the capture sets of its parents
      for parent <- impl.parents do // (1)
        checkSubset(capturedVars(parent.tpe.classSymbol), localSet, parent.srcPos,
          i"\nof the references allowed to be captured by $cls")

      def checkExplicitUses(sym: Symbol): Unit = capturedVars(sym) match
        case cs: CaptureSet.Var
        if cs.elems.exists(!_.isTerminalCapability) && !isExemptFromExplicitChecks(sym) =>
          val usesStr = if sym.isClass then "uses" else "uses_init"
          report.error(
            em"""Publicly visible $sym uses external capabilities $cs.
               |These dependencies need to be declared explicitly in a `$usesStr ...` clause.""",
            sym.srcPos)
        case _ =>

      val saved = curEnv
      curEnv = Env(cls, EnvKind.Regular, localSet, curEnv)
      try
        // (2) Capture set of self type includes capture set of class
        val thisSet = cls.classInfo.selfType.captureSet.withDescription(i"of the self type of $cls")
        checkSubset(localSet, thisSet, tree.srcPos)

        // (3) Capture set of self type includes capture sets of tracked parameters
        for param <- cls.paramGetters do
          if param.isTrackedParamAccessor then
            withGlobalCapAsRoot: // OK? We need this here since self types use GlobalAny instead of a LocalCap
              checkSubset(param.termRef.captureSet, thisSet, param.srcPos)

        // (3b) Capture set of self type includes capture sets of fields (including fresh)
        withGlobalCapAsRoot:
          checkSubset(captureSetImpliedByFields(cls, cls.appliedRef), thisSet, tree.srcPos)

        // (4) If class extends Pure, capture set of self type is empty
        for pureBase <- cls.pureBaseClass do // (4)
          def selfTypeTree = impl.body
            .collect:
              case TypeDef(tpnme.SELF, rhs) => rhs
            .headOption
            .getOrElse(tree)  // Use class tree if self type tree is missing ...
            .orElse(tree)     // ... or empty.
          checkSubset(thisSet,
            CaptureSet.empty.withDescription(i"of pure base class $pureBase"),
            selfTypeTree.srcPos, cs1description = " captured by this self type")

        // (5) Check AppliedType parents
        for case tpt: TypeTree <- impl.parents do
          tpt.tpe match
            case AppliedType(fn, args) =>
              markFreeTypeArgs(tpt, fn.typeSymbol, args.map(TypeTree(_)))
            case _ =>

        super.recheckClassDef(tree, impl, cls)
      finally
        checkExplicitUses(cls)
        checkExplicitUses(cls.primaryConstructor)
        if cls.is(ModuleClass) then
          interpolate(cls.sourceModule.info, cls.sourceModule)
        completed += cls
        curEnv = saved
    end recheckClassDef

    /** If type is of the form `T @requiresCapability(x)`,
     *  mark `x` as free in the current environment. This is used to require the
     *  correct `CanThrow` capability when encountering a `throw`.
     */
    override def recheckTyped(tree: Typed)(using Context): Type =
      tree.tpt.tpe match
        case AnnotatedType(_, annot) if annot.symbol == defn.RequiresCapabilityAnnot =>
          annot.tree match
            case Apply(_, cap :: Nil) =>
              markFree(cap.symbol, tree)
            case _ =>
        case _ =>
      super.recheckTyped(tree)

    /** Under the sealed policy and with saferExceptions, disallow `any` in the
     *  result type of a try
     */
    override def recheckTry(tree: Try, pt: Type)(using Context): Type =
      val tryOwner = Setup.firstCanThrowEvidence(tree.expr) match
        case Some(vd) => vd.symbol.owner
        case None => ctx.owner
      val bodyType = inContext(ctx.withOwner(tryOwner)):
        recheck(tree.expr, pt)
      val tp = recheckTryRest(bodyType, tree.cases, tree.finalizer, pt)
      if Feature.enabled(Feature.saferExceptions) then
        disallowBadRootsIn(tp, ctx.owner,
          "The result of `try`", "have type",
          "\nThis is often caused by a locally generated exception capability leaking as part of its result.",
          tree.srcPos)
      tp

    /* Currently not needed, since capture checking takes place after ElimByName.
     * Keep around in case we need to get back to it
    def recheckByNameArg(tree: Tree, pt: Type)(using Context): Type =
      val closureDef(mdef) = tree: @unchecked
      val arg = mdef.rhs
      val localSet = CaptureSet.Var()
      curEnv = Env(mdef.symbol, localSet, isBoxed = false, curEnv)
      val result =
        try
          inContext(ctx.withOwner(mdef.symbol)) {
            recheckStart(arg, pt).capturing(localSet)
          }
        finally curEnv = curEnv.outer
      recheckFinish(result, arg, pt)
    */

    /** If environment is owned by a class, run in a new environment owned by
     *  its primary constructor instead.
     */
    def pushConstructorEnv()(using Context): Unit =
      if curEnv.owner.isClass then
        val constr = curEnv.owner.primaryConstructor
        if constr.exists then
          val constrSet = capturedVars(constr)
          if capturedVars(constr) ne CaptureSet.empty then
            curEnv = Env(constr, EnvKind.Regular, constrSet, curEnv)

    override def recheckStat(stat: Tree)(using Context): Unit =
      val saved = curEnv
      if !stat.isInstanceOf[MemberDef] then
        pushConstructorEnv()
      try recheck(stat)
      finally curEnv = saved

    /** The main recheck method does some box adapation for all nodes:
     *   - If expected type `pt` is boxed and the tree is a lambda or a reference,
     *     don't propagate free variables.
     *   - If the expected type is not boxed but the result type is boxed,
     *     simulate an unboxing by adding all references in the boxed capture set
     *     of the result type to the current environment.
     */
    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val saved = curEnv
      tree match
        case _: RefTree | closureDef(_) if pt.isBoxedCapturing =>
          curEnv = Env(curEnv.owner, EnvKind.Boxed,
            CaptureSet.Var(curEnv.owner), curEnv)
        case _ =>
      val res =
        try
          if capt eq noPrinter then
            super.recheck(tree, pt)
          else
            trace.force(i"rechecking $tree with pt = $pt", recheckr, show = true):
              super.recheck(tree, pt)
        catch case ex: AssertionError =>
          println(i"error while rechecking $tree against $pt")
          throw ex
        finally curEnv = saved
      if tree.isTerm && !pt.isBoxedCapturing && pt != LhsProto then
        markFree(res.boxedCaptureSet, tree)
      res
    end recheck

    // ------------------ Adaptation -------------------------------------
    //
    // Adaptations before checking conformance of actual vs expected:
    //
    //   - Convert function to dependent function if expected type is a dependent function type
    //     (c.f. alignDependentFunction).
    //   - Relax expected capture set containing `this.type`s by adding references only
    //     accessible through those types (c.f. addOuterRefs, also #14930 for a discussion).
    //   - Adapt box status and environment capture sets by simulating box/unbox operations.
    //   - Instantiate covariant occurrenves of `any` in actual to reach capabilities.

    private inline val debugSuccesses = false

    type BoxErrors = mutable.ListBuffer[Message] | Null

    /** Addendas for error messages that show where we have under-approximated by
     *  mapping of a capability in contravariant position to the empty set because
     *  the original result type of the map was not itself a capability.
     */
    private def addApproxAddenda(using Context): TypeAccumulator[List[Note]] =
      new TypeAccumulator:
        def apply(notes: List[Note], t: Type) = t match
          case CapturingType(t, CaptureSet.EmptyWithProvenance(ref, mapped)) =>
            /* val (origCore, kind) = original match
              case tp @ AnnotatedType(parent, ann) if ann.hasSymbol(defn.ReachCapabilityAnnot) =>
                (parent, " deep")
              case _ =>
                (original, "")*/
            Note:
                i"""
                   |
                   |Note that a capability $ref in a capture set appearing in contravariant position
                   |was mapped to $mapped which is not a capability. Therefore, it was under-approximated to the empty set."""
            :: notes
          case _ =>
            foldOver(notes, t)

    /** Massage `actual` and `expected` types before checking conformance.
     *  Massaging is done by the methods following this one:
     *   - align dependent function types and add outer references in the expected type
     *   - adapt boxing in the actual type
     *  If the resulting types are not compatible, try again with an actual type
     *  where local capture roots are instantiated to root variables.
     */
    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree, notes: List[Note])(using Context): Type =
      try testAdapted(actual, expected, tree, notes)(err.typeMismatch)
      catch case ex: AssertionError =>
        println(i"error while checking $tree: $actual against $expected")
        throw ex

    @annotation.tailrec
    private def findImpureUpperBound(tp: Type)(using Context): Type = tp match
      case _: SingletonType => findImpureUpperBound(tp.widen)
      case tp: TypeRef if tp.symbol.isAbstractOrParamType =>
        tp.info match
          case TypeBounds(_, hi) if hi.isBoxedCapturing => hi
          case TypeBounds(_, hi) => findImpureUpperBound(hi)
          case _ => NoType
      case _ => NoType

    inline def testAdapted(actual: Type, expected: Type, tree: Tree, notes: List[Note])
        (fail: (Tree, Type, List[Note]) => Unit)(using Context): Type =

      var expected1 = alignDependentFunction(expected, actual.stripCapturing)
      val falseDeps = expected1 ne expected
      val actual1 =
        if expected.stripCapturing.isInstanceOf[SelectionProto] then
          // If the expected type is a `SelectionProto`, we should be careful about cases when
          // the actual type is a type parameter (for instance, `X <: box IO^`).
          // If `X` were not widen to reveal the boxed type, both sides are unboxed and thus
          // no box adaptation happens. But it is unsound: selecting a member from `X` implicitly
          // unboxes the value.
          //
          // Therefore, when the expected type is a selection proto, we conservatively widen
          // the actual type to strip type parameters.
          val hi = findImpureUpperBound(actual)
          if !hi.exists then actual else hi
        else actual
      val actualBoxed = adapt(actual1, expected1, tree)
      //println(i"check conforms $actualBoxed <<< $expected1")

      if actualBoxed eq actual then
        // Only `addOuterRefs` when there is no box adaptation
        expected1 = addOuterRefs(expected1, actual, tree.srcPos)

      def tryCurrentType: Boolean =
        isCompatible(actualBoxed, expected1)

      /** When the actual type is a named type, and the previous attempt failed, try to widen the named type
       * and try another time.
       *
       * This is useful for cases like:
       *
       *   def id[X <: box IO^{a}](x: X): IO^{a} = x
       *
       * When typechecking the body, we need to show that `(x: X)` can be typed at `IO^{a}`.
       * In the first attempt, since `X` is simply a parameter reference, we treat it as non-boxed and perform
       * no box adptation. But its upper bound is in fact boxed, and adaptation is needed for typechecking the body.
       * In those cases, we widen such types and try box adaptation another time.
       */
      def tryWidenNamed: Boolean =
        val actual1 = findImpureUpperBound(actual)
        actual1.exists && {
          val actualBoxed1 = adapt(actual1, expected1, tree)
          isCompatible(actualBoxed1, expected1)
        }

      TypeComparer.compareResult(tryCurrentType || tryWidenNamed) match
        case TypeComparer.CompareResult.Fail(cmpNotes) =>
          capt.println(i"conforms failed for ${tree}: $actual vs $expected")
          if falseDeps then expected1 = unalignFunction(expected1)
          val toAdd0 = notes ++ cmpNotes
          val toAdd1 = addApproxAddenda(toAdd0, expected1)
          val failTree =
            if definedSym(tree).exists then tree else tree.withType(actualBoxed)
          fail(failTree, expected1, toAdd1)
          actual
        case /*OK*/ _ =>
          if debugSuccesses then tree match
            case Ident(_) =>
              println(i"SUCCESS $tree for $actual <:< $expected:\n${TypeComparer.explained(_.isSubType(actualBoxed, expected1))}")
            case _ =>
          actualBoxed
    end testAdapted

    /** Turn `expected` into a dependent function when `actual` is dependent. */
    private def alignDependentFunction(expected: Type, actual: Type)(using Context): Type =
      def recur(expected: Type): Type = expected.dealias match
        case expected @ CapturingType(eparent, refs) =>
          expected.derivedCapturingType(recur(eparent), refs)
        case expected @ defn.FunctionOf(args, resultType, isContextual)
        if defn.isNonRefinedFunction(expected) =>
          actual match
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              depFun(args, resultType, isContextual, rinfo.paramNames)
            case _ => expected
        case expected @ defn.RefinedFunctionOf(einfo: MethodType)
        if einfo.allParamNamesSynthetic =>
          actual match
            case defn.RefinedFunctionOf(ainfo: MethodType)
            if !ainfo.allParamNamesSynthetic && ainfo.paramNames.hasSameLengthAs(einfo.paramNames) =>
              einfo.derivedLambdaType(paramNames = ainfo.paramNames)
                .toFunctionType(alwaysDependent = true)
            case _ => expected
        case _ => expected
      recur(expected)

    private def unalignFunction(tp: Type)(using Context): Type = tp match
      case tp @ CapturingType(parent, refs) =>
        tp.derivedCapturingType(unalignFunction(parent), refs)
      case defn.RefinedFunctionOf(mt) =>
        mt.toFunctionType(alwaysDependent = false)
      case _ =>
        tp

    /** For the expected type, implement the rule outlined in #14390:
     *   - when checking an expression `a: Ta^Ca` against an expected type `Te^Ce`,
     *   - where the capture set `Ce` contains Cls.this,
     *   - and where all method definitions enclosing `a` inside class `Cls`
     *     have only pure parameters,
     *   - add to `Ce` all references to variables or this-references in `Ca`
     *     that are outside `Cls`. These are all accessed through `Cls.this`,
     *     so we can assume they are already accounted for by `Ce` and adding
     *     them explicitly to `Ce` changes nothing.
     *   - To make up for this, we also add these variables to the capture set of `Cls`,
     *     so that all instances of `Cls` will capture these outer references.
     *  So in a sense we use `{Cls.this}` as a placeholder for certain outer captures.
     *  that we needed to be subsumed by `Cls.this`.
     */
    private def addOuterRefs(expected: Type, actual: Type, pos: SrcPos)(using Context): Type =

      def isPure(info: Type): Boolean = info match
        case info: PolyType => isPure(info.resType)
        case info: MethodType => info.paramInfos.forall(_.captureSet.isAlwaysEmpty) && isPure(info.resType)
        case _ => true

      def isPureContext(owner: Symbol, limit: Symbol): Boolean =
        if owner == limit then true
        else if !owner.exists then false
        else isPure(owner.info) && isPureContext(owner.owner, limit)

      // Augment expected capture set `erefs` by all references in actual capture
      // set `arefs` that are outside some `C.this.type` reference in `erefs` for an enclosing
      // class `C`. If an added reference is not a ThisType itself, add it to the capture set
      // (i.e. use set) of the `C`. This makes sure that any outer reference implicitly subsumed
      // by `C.this` becomes a capability of every instance of `C`.
      def augment(erefs: CaptureSet, arefs: CaptureSet): CaptureSet =
        (erefs /: erefs.elems): (erefs, eref) =>
          eref match
            case eref: ThisType if isPureContext(ctx.owner, eref.cls) =>
              val outerRefs = arefs.filter: aref =>
                eref.cls.isProperlyContainedIn(aref.pathOwner)
              // Include implicitly added outer references in the capture set of the class of `eref`.
              for outerRef <- outerRefs.elems do
                if !erefs.elems.contains(outerRef)
                    && !outerRef.pathRoot.isInstanceOf[ThisType]
                    // we don't need to add outer ThisTypes as these are anyway added as path
                    // prefixes at the use site. And this exemption is required since capture sets
                    // of non-local classes are always empty, so we can't add an outer this to them.
                then
                  def provenance =
                    i""" of the enclosing class ${eref.cls}.
                       |The reference was included since we tried to establish that $arefs <: $erefs"""
                  checkElem(outerRef, capturedVars(eref.cls), pos, provenance)

              erefs ++ outerRefs
            case _ =>
              erefs

      expected match
        case CapturingType(ecore, erefs) =>
          val erefs1 = augment(erefs, actual.captureSet)
          if erefs1 ne erefs then
            capt.println(i"augmented $expected from ${actual.captureSet} --> $erefs1")
          expected.derivedCapturingType(ecore, erefs1)
        case _ =>
          expected
    end addOuterRefs

    /** A debugging method for showing the envrionments during capture checking. */
    private def debugShowEnvs()(using Context): Unit =
      def showEnv(env: Env): String = i"Env(${env.owner}, ${env.kind}, ${env.captured})"
      val sb = StringBuilder()
      @annotation.tailrec def walk(env: Env): Unit =
        if !env.isRoot then
          sb ++= showEnv(env)
          sb ++= "\n"
          walk(env.outer)
      sb ++= "===== Current Envs ======\n"
      walk(curEnv)
      sb ++= "===== End          ======\n"
      println(sb.result())

    /** Adapt `actual` type to `expected` type by inserting boxing and unboxing conversions
     *
     *  @param alwaysConst  always make capture set variables constant after adaptation
     */
    def adaptBoxed(actual: Type, expected: Type, tree: Tree, covariant: Boolean, alwaysConst: Boolean)(using Context): Type =

      def recur(actual: Type, expected: Type, covariant: Boolean): Type =

        /** Adapt the inner shape type: get the adapted shape type, and the capture set leaked during adaptation
         *  @param boxed   if true we adapt to a boxed expected type
         */
        def adaptShape(actualShape: Type, boxed: Boolean): (Type, CaptureSet) = actualShape match
          case FunctionOrMethod(aargs, ares) =>
            val saved = curEnv
            curEnv = Env(
              curEnv.owner,
              if boxed then EnvKind.Boxed else EnvKind.NestedInOwner,
              CaptureSet.Var(curEnv.owner),
              if boxed then null else curEnv)
            try
              val (eargs, eres) = expected.dealias.stripCapturing match
                case FunctionOrMethod(eargs, eres) => (eargs, eres)
                case _ => (aargs.map(_ => WildcardType), WildcardType)
              val aargs1 = aargs.zipWithConserve(eargs):
                recur(_, _, !covariant)
              val ares1 = recur(ares, eres, covariant)
              val resTp =
                if (aargs1 eq aargs) && (ares1 eq ares) then actualShape // optimize to avoid redundant matches
                else actualShape.derivedFunctionOrMethod(aargs1, ares1)
              (resTp, CaptureSet(curEnv.captured.elems))
            finally curEnv = saved
          case _ =>
            (actualShape, CaptureSet())
        end adaptShape

        //val adaptStr = i"adapting $actual ${if covariant then "~~>" else "<~~"} $expected"
        //println(adaptStr)

        // Get wildcards out of the way
        expected match
          case _: WildcardType => return actual
          case _ =>

        // Decompose the actual type into the inner shape type, the capture set and the box status
        val actualShape = if actual.isFromJavaObject then actual else actual.stripCapturing
        val actualIsBoxed = actual.isBoxedCapturing

        // A box/unbox should be inserted, if the actual box status mismatches with the expectation
        val needsAdaptation = actualIsBoxed != expected.isBoxedCapturing
        // Whether to insert a box or an unbox?
        val insertBox = needsAdaptation && covariant != actualIsBoxed

        // Adapt the inner shape type: get the adapted shape type, and the capture set leaked during adaptation
        val (adaptedShape, leaked) = adaptShape(actualShape, insertBox)

        // Capture set of the term after adaptation
        val captures =
          val cs = actual.captureSet
          if covariant then cs ++ leaked
          else
            if !leaked.subCaptures(cs) then
              report.error(
                em"""$expected cannot be box-converted to ${actual.capturing(leaked)}
                    |since the additional capture set $leaked resulting from box conversion is not allowed in $actual""", tree.srcPos)
            cs

        def adaptedType(resultBoxed: Boolean) =
          if (adaptedShape eq actualShape) && leaked.isAlwaysEmpty && actualIsBoxed == resultBoxed
          then actual
          else adaptedShape
            .capturing(if alwaysConst then CaptureSet(captures.elems) else captures)
            .forceBoxStatus(resultBoxed)

        if needsAdaptation && !insertBox then // we are unboxing
          val criticalSet =          // the set with which we unbox
            if covariant then
              if expected.expectsReadOnly && actual.derivesFromStateful
              then captures.readOnly
              else captures
            else expected.captureSet     // contravarant: we unbox with captures of epected type
            //debugShowEnvs()
          markFree(criticalSet, tree)

        // Compute the adapted type.
        // The result is boxed if actual is boxed and we don't need to adapt,
        // or if actual is unboxed and we do need to adapt.
        val resultIsBoxed = actualIsBoxed != needsAdaptation
        if (adaptedShape eq actualShape) && leaked.isAlwaysEmpty && actualIsBoxed == resultIsBoxed
          then actual
          else adaptedShape
            .capturing(if alwaysConst then CaptureSet(captures.elems) else captures)
            .forceBoxStatus(resultIsBoxed)
      end recur

      recur(actual, expected, covariant)
    end adaptBoxed

    /** If actual is a tracked Capability `a` and widened is a capturing type T^C,
     *  improve `T^C` to `T^{a}`, following the VAR rule of CC.
     *  TODO: We probably should do this also for other top-level occurrences of captures
     *  E.g.
     *    class Foo { def a: C^{io}; val def: C^{async} }
     *    val foo: Foo^{io, async}
     *  Then
     *    foo: Foo { def a: C^{foo}; def b: C^{foo} }^{foo}
     */
    private def improveCaptures(widened: Type, prefix: Type)(using Context): Type = prefix match
      case ref: Capability if ref.isTracked =>
        widened match
          case widened @ CapturingType(p, refs)
          if ref.singletonCaptureSet.mightSubcapture(refs) && !widened.isBoxed =>
            widened.derivedCapturingType(p, ref.singletonCaptureSet)
              .showing(i"improve $widened to $result", capt)
          case _ => widened
      case _ => widened

    /* Currently not needed since it forms part of `adapt`
    private def improve(actual: Type, prefix: Type)(using Context): Type =
      val widened = actual.widen.dealiasKeepAnnots
      val improved = improveCaptures(widened, prefix).withReachCaptures(prefix)
      if improved eq widened then actual else improved
    */

    /** An actual singleton type should not be widened if the expected type is a
     *  LhsProto, or a singleton type, or a path selection with a stable value
     */
    private def noWiden(actual: Type, expected: Type)(using Context): Boolean =
      actual.isSingleton
      && expected.match
          case expected: PathSelectionProto => !expected.select.symbol.isOneOf(UnstableValueFlags)
          case _ => expected.stripCapturing.isSingleton || expected == LhsProto

    /** Adapt `actual` type to `expected` type. This involves:
     *   - narrow toplevel captures of `x`'s underlying type to `{x}` according to CC's VAR rule
     *   - narrow nested captures of `x`'s underlying type to `{x*}`
     *   - do box adaptation
     */
    def adapt(actual: Type, expected: Type, tree: Tree)(using Context): Type =
      if noWiden(actual, expected) then
        expected match
          case expected @ CapturingType(_, _) if expected.isBoxed =>
            // actual is a singleton type and expected is of the form box x.type^cs.
            // Convert actual to the same form.
            actual.boxDeeply
              .showing(i"adapt single $actual / $result vs $expected", capt)
          case _ =>
            actual
      else
        // Compute the widened type. Drop `@use` and `@consume` annotations from the type,
        // since they obscures the capturing type.
        val widened = actual.widen.dealiasKeepAnnots.dropUseAndConsumeAnnots
        val improvedVAR = improveCaptures(widened, actual)
        val adaptedReadOnly = adaptReadOnly(improvedVAR, actual, expected, tree)
        val adapted = adaptBoxed(
            adaptedReadOnly.withReachCaptures(actual), expected, tree,
            covariant = true, alwaysConst = false)
        if adapted eq improvedVAR // no read-only-adaptation, no reaches added, no box-adaptation
        then actual               // might as well use actual instead of improved widened
        else adapted.showing(i"adapt $actual vs $expected = $adapted", capt)
    end adapt

// ---- Unit-level rechecking -------------------------------------------

    /** Check overrides again, taking capture sets into account.
    *  TODO: Can we avoid doing overrides checks twice?
    *  We need to do them here since only at this phase CaptureTypes are relevant
    *  But maybe we can then elide the check during the RefChecks phase under captureChecking?
    */
    def checkOverrides = new TreeTraverser:
      class OverridingPairsCheckerCC(clazz: ClassSymbol, self: Type, tree: Tree)(using Context) extends OverridingPairsChecker(clazz, self):

        /** Omit the check if one of {overriding,overridden} was nnot capture checked */
        override def needsCheck(overriding: Symbol, overridden: Symbol)(using Context): Boolean =
          !setup.isPreCC(overriding) && !setup.isPreCC(overridden)

        /** Perform box adaptation for override checking */
        override def adaptOverridePair(member: Symbol, memberTp: Type, otherTp: Type)(using Context): Option[(Type, Type)] =
          if member.isType then
            memberTp match
              case TypeAlias(_) =>
                otherTp match
                  case otherTp: RealTypeBounds =>
                    if otherTp.hi.isBoxedCapturing || otherTp.lo.isBoxedCapturing then
                      Some((memberTp, otherTp.unboxed))
                    else otherTp.hi match
                      case hi @ CapturingType(parent: TypeRef, refs)
                      if parent.symbol == defn.Caps_CapSet && refs.isUniversal =>
                        Some((
                          memberTp,
                          otherTp.derivedTypeBounds(
                            otherTp.lo,
                            hi.derivedCapturingType(parent,
                                LocalCap(Origin.OverriddenType(member)).singletonCaptureSet))))
                      case _ => None
                  case _ => None
              case _ => None
          else memberTp match
            case memberTp @ ExprType(memberRes) =>
              adaptOverridePair(member, memberRes, otherTp) match
                case Some((mres, otp)) => Some((memberTp.derivedExprType(mres), otp))
                case None => None
            case _ => otherTp match
              case otherTp @ ExprType(otherRes) =>
                adaptOverridePair(member, memberTp, otherRes) match
                  case Some((mtp, ores)) => Some((mtp, otherTp.derivedExprType(ores)))
                  case None => None
              case _ =>
                val expected1 = alignDependentFunction(addOuterRefs(otherTp, memberTp, tree.srcPos), memberTp.stripCapturing)
                val actual1 =
                  val saved = curEnv
                  try
                    curEnv = Env(clazz, EnvKind.NestedInOwner, capturedVars(clazz), outer0 = curEnv)
                    val adapted =
                      adaptBoxed(memberTp, expected1, tree, covariant = true, alwaysConst = true)
                    memberTp match
                      case _: MethodType =>
                        // We remove the capture set resulted from box adaptation for method types,
                        // since class methods are always treated as pure, and their captured variables
                        // are charged to the capture set of the class (which is already done during
                        // box adaptation).
                        adapted.stripCapturing
                      case _ => adapted
                  finally curEnv = saved
                if (actual1 eq memberTp) && (expected1 eq otherTp) then None
                else Some((actual1, expected1))
        end adaptOverridePair

        override def checkInheritedTraitParameters: Boolean = false

        /** Check that overrides don't change the @use, @consume, or @reserve status of their parameters */
        override def additionalChecks(member: Symbol, other: Symbol)(using Context): Unit =
          for
            (params1, params2) <- member.rawParamss.lazyZip(other.rawParamss)
            (param1, param2) <- params1.lazyZip(params2)
          do
            def checkAnnot(cls: ClassSymbol) =
              if param1.hasAnnotation(cls) != param2.hasAnnotation(cls) then
                report.error(
                  OverrideError(
                      i"has a parameter ${param1.name} with different @${cls.name} status than the corresponding parameter in the overridden definition",
                      self, member, other, self.memberInfo(member), self.memberInfo(other)
                    ),
                  if member.owner == clazz then member.srcPos else clazz.srcPos)

            checkAnnot(defn.UseAnnot)
            checkAnnot(defn.ConsumeAnnot)
            checkAnnot(defn.ReserveAnnot)
      end OverridingPairsCheckerCC

      def traverse(t: Tree)(using Context) =
        t match
          case t: Template =>
            withCollapsedLocalCaps:
              checkAllOverrides(ctx.owner.asClass, OverridingPairsCheckerCC(_, _, t))
          case _ =>
        traverseChildren(t)
    end checkOverrides

    private val setup: SetupAPI = thisPhase.prev.asInstanceOf[Setup]

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      capt.println(i"cc check ${unit.source}")
      ccState.start()
      setup.setupUnit(unit.tpdTree, this)

      if ctx.settings.YccPrintSetup.value then
        val echoHeader = "[[syntax tree at end of cc setup]]"
        val treeString = show(unit.tpdTree)
        report.echo(s"$echoHeader\n$treeString\n")

      withCaptureSetsExplained:
        def iterate(): Unit =
          super.checkUnit(unit)
          if !ctx.reporter.errorsReported
              && (needAnotherRun
                  || ccConfig.alwaysRepeatRun && ccState.iterationId == 1)
          then
            resetIteration()
            ccState.nextIteration:
              setup.setupUnit(unit.tpdTree, this)
              capt.println(s"**** capture checking run ${ccState.iterationId} started on ${ctx.source}")
              iterate()

        iterate()
        checkOverrides.traverse(unit.tpdTree)
        postCheck(unit.tpdTree)
        checkSelfTypes(unit.tpdTree)
        postCheckWF(unit.tpdTree)
        if ctx.settings.YccDebug.value then
          show(unit.tpdTree) // this does not print tree, but makes its variables visible for dependency printing
    end checkUnit

// ----- Checks to do after the rechecking traversal --------------------------

    /** Check that self types of subclasses conform to self types of super classes.
     *  (See comment below how this is achieved). The check assumes that classes
     *  without an explicit self type have the universal capture set `{caps.any}` on the
     *  self type. If a class without explicit self type is not `effectivelySealed`
     *  it is checked that the inferred self type is universal, in order to assure
     *  that joint and separate compilation give the same result.
     */
    def checkSelfTypes(unit: tpd.Tree)(using Context): Unit =
      val parentTrees = mutable.HashMap[Symbol, List[Tree]]()
      unit.foreachSubTree {
        case cdef @ TypeDef(_, impl: Template) => parentTrees(cdef.symbol) = impl.parents
        case _ =>
      }
      // Perform self type checking. The problem here is that `checkParents` compares a
      // self type of a subclass with the result of an asSeenFrom of the self type of the
      // superclass. That's no good. We need to constrain the original superclass self type
      // capture set, not the set mapped by asSeenFrom.
      //
      // Instead, we proceed from parent classes to child classes. For every class
      // we first check its parents, and then interpolate the self type to an
      // upper approximation that satisfies all constraints on its capture set.
      // That means all capture sets of parent self types are constants, so mapping
      // them with asSeenFrom is OK.
      while parentTrees.nonEmpty do
        val roots = parentTrees.keysIterator.filter {
          cls => !parentTrees(cls).exists(ptree => parentTrees.contains(ptree.tpe.classSymbol))
        }
        assert(roots.nonEmpty)
        for case root: ClassSymbol <- roots do
          inContext(ctx.fresh.setOwner(root)):
            checkSelfAgainstParents(root, root.baseClasses)
            val selfType = root.asClass.classInfo.selfType
            interpolate(selfType, root, startingVariance = -1)
            selfType match
              case CapturingType(_, refs: CaptureSet.Var)
              if !root.isEffectivelySealed
                  && !refs.isUniversal
                  && !root.matchesExplicitRefsInBaseClass(refs)
              =>
                // Forbid inferred self types unless they are already implied by an explicit
                // self type in a parent.
                report.error(
                  em"""$root needs an explicitly declared self type since its
                      |inferred self type $selfType
                      |is not visible in other compilation units that define subclasses.""",
                  root.srcPos)
              case _ =>
            parentTrees -= root
            capt.println(i"checked $root with $selfType")
    end checkSelfTypes

    /** Check ill-formed capture sets in a type parameter. We used to be able to
     *  push parameter refs into a capture set in type parameters that this type
     *  parameter can't see. We used to heal this by replacing illegal refs by their
     *  underlying capture sets. But now these should no longer be necessary, so
     *  instead of errors we use assertions.
     */
    private def checkTypeParam(tree: Tree, paramName: TypeName, meth: Symbol)(using Context): Unit =
      val checker = new TypeTraverser:
        private var allowed: SimpleIdentitySet[TermParamRef] = SimpleIdentitySet.empty

        private def checkCaptureSet(cs: CaptureSet): Unit =
          for elem <- cs.elems do
            elem.stripReach match
              case ref: TermParamRef => assert(allowed.contains(ref))
              case _ =>

        def traverse(tp: Type) =
          tp match
            case CapturingType(parent, refs) =>
              checkCaptureSet(refs)
              traverse(parent)
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              traverse(rinfo)
            case tp: TermLambda =>
              val saved = allowed
              try
                tp.paramRefs.foreach(allowed += _)
                traverseChildren(tp)
              finally allowed = saved
            case _ =>
              traverseChildren(tp)

      if tree.isInstanceOf[InferredTypeTree] then
        checker.traverse(tree.nuType)
    end checkTypeParam

    /** Check that no uses refer to reach capabilities of parameters of enclosing
     *  methods or classes.
     */
    def checkEscapingUses()(using Context) =
      for (tree, uses, env) <- useInfos do
        val seen = util.EqHashSet[Capability]()

        // The owner of the innermost environment of kind Boxed
        def boxedOwner(env: Env): Symbol =
          if env.kind == EnvKind.Boxed then env.owner
          else if isOfNestedMethod(env) then env.owner.owner
          else
            val nextEnv = nextEnvToCharge(env)
            if nextEnv.isRoot then NoSymbol else boxedOwner(nextEnv)

        def checkUseUnlessBoxed(c: Capability, croot: NamedType) =
          if !boxedOwner(env).isContainedIn(croot.symbol.owner) then
            checkUseDeclared(c, tree.srcPos)

        def check(cs: CaptureSet): Unit = cs.elems.foreach(checkElem)

        def checkElem(c: Capability): Unit =
          if !seen.contains(c) then
            seen += c
            c match
              case Reach(c1) =>
                c1.paramPathRoot match
                  case croot: NamedType => checkUseUnlessBoxed(c, croot)
                  case _ => check(CaptureSet.ofTypeDeeply(c1.widen))
              case c: TypeRef =>
                c.paramPathRoot match
                  case croot: NamedType => checkUseUnlessBoxed(c, croot)
                  case _ =>
              case c: DerivedCapability =>
                checkElem(c.underlying)
              case c: LocalCap =>
                c.origin match
                  case Origin.Parameter(param) =>
                    report.error(
                      em"Local $c created in type of $param leaks into capture scope${param.owner.qualString("of")}",
                      tree.srcPos)
                  case _ =>
                    check(c.hiddenSet)
              case _ =>

        check(uses)
      end for
    end checkEscapingUses

    /** Check all parent class constructors of classes extending Stateful
     *  either also extend Stateful or are read-only.
     *
     *  A parent class constructor is _read-only_ if the following conditions are met
     *   1. The class does not retain any exclusive capabilities from its environment.
     *   2. The constructor does not take arguments that retain exclusive capabilities.
     *   3. The class does not does not have fields that retain exclusive universal capabilities.
     */
    def checkStatefulInheritance(cls: ClassSymbol, parents: List[Tree])(using Context): Unit =
      if cls.derivesFrom(defn.Caps_Stateful) then
        for parent <- parents do
          if !parent.tpe.derivesFromStateful then
            val pcls = parent.nuType.classSymbol
            val parentIsExclusive =
              if parent.isType then
                capturedVars(pcls).isExclusive()
                || captureSetImpliedByFields(pcls.asClass, parent.nuType).isExclusive()
              else parent.nuType.captureSet.isExclusive()
            if parentIsExclusive then
              report.error(
                em"""illegal inheritance: $cls which extends `Stateful` is not allowed to also extend $pcls
                    |since $pcls retains exclusive capabilities but does not extend `Stateful`.""",
                parent.srcPos)

    /** Checks to run after the rechecking pass:
     *   - Check that arguments of TypeApplys and AppliedTypes conform to their bounds.
     *   - Check that no uses refer to reach capabilities of parameters of enclosing
     *     methods or classes.
     *   - Run the separation checker under language.experimental.separationChecking
     *   - Check that classes extending Stateful do not extend other classes that do
     *     not extend Stateful yet retain exclusive capabilities
     */
    def postCheck(unit: tpd.Tree)(using Context): Unit =
      val checker = new TreeTraverser:
        def traverse(tree: Tree)(using Context): Unit =
          val lctx = tree match
            case _: DefTree | _: TypeDef if tree.symbol.exists => ctx.withOwner(tree.symbol)
            case _ => ctx
          trace(i"post check $tree"):
            traverseChildren(tree)(using lctx)
            check(tree)

        def check(tree: Tree)(using Context) = tree match
          case TypeApply(fun, args) =>
            fun.nuType.widen match
              case tl: PolyType =>
                val normArgs = args.lazyZip(tl.paramInfos).map: (arg, bounds) =>
                  arg.withType(arg.nuType.forceBoxStatus(
                    bounds.hi.isBoxedCapturing | bounds.lo.isBoxedCapturing))
                withCollapsedLocalCaps: // OK? We need this since bounds use GlobalAny instead of LocalCap
                  // TODO Do bounds still contain GlobalAny?
                  checkBounds(normArgs, tl)
                if ccConfig.postCheckCapturesets then
                  args.lazyZip(tl.paramNames).foreach(checkTypeParam(_, _, fun.symbol))
              case _ =>
          case TypeDef(_, impl: Template) =>
            checkStatefulInheritance(tree.symbol.asClass, impl.parents)
          case _ =>
      end checker

      checker.traverse(unit)(using ctx.withOwner(defn.RootClass))
      checkEscapingUses()
      if sepChecksEnabled then
        for (tree, cs, env) <- useInfos do
          usedSet(tree) = tree.markedFree ++ cs
        ccState.inSepCheck:
          SepCheck(this).traverse(unit)

      if !ctx.reporter.errorsReported then
        // We dont report errors here if previous errors were reported, because other
        // errors often result in bad applied types, but flagging these bad types gives
        // often worse error messages than the original errors.
        val checkAppliedTypes = new TreeTraverser:
          def traverse(t: Tree)(using Context) = t match
            case tree: InferredTypeTree =>
            case tree: New =>
            case tree: TypeTree =>
              withCollapsedLocalCaps:
                checkAppliedTypesIn(tree.withType(tree.nuType))
            case _ => traverseChildren(t)
        checkAppliedTypes.traverse(unit)
    end postCheck

    /** Perform the following kinds of checks:
     *   - Check all explicitly written capturing types for well-formedness using `checkWellFormedPost`.
     *   - Check that publicly visible inferred types correspond to the type
     *     they have without capture checking.
     */
    def postCheckWF(unit: tpd.Tree)(using Context): Unit =
      for chk <- todoAtPostCheck do chk()
      setup.postCheck()
  end CaptureChecker
end CheckCaptures
