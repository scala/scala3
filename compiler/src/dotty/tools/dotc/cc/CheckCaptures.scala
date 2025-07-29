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
import typer.RefChecks.{checkAllOverrides, checkSelfAgainstParents, OverridingPairsChecker}
import typer.Checking.{checkBounds, checkAppliedTypesIn}
import typer.ErrorReporting.{Addenda, NothingToAdd, err}
import typer.ProtoTypes.{LhsProto, WildcardSelectionProto}
import util.{SimpleIdentitySet, EqHashMap, EqHashSet, SrcPos, Property}
import transform.{Recheck, PreRecheck, CapturedVars}
import Recheck.*
import scala.collection.mutable
import CaptureSet.{withCaptureSetsExplained, IncludeFailure, ExistentialSubsumesFailure}
import CCState.*
import StdNames.nme
import NameKinds.{DefaultGetterName, WildcardParamName, UniqueNameKind}
import reporting.{trace, Message, OverrideError}
import Annotations.Annotation
import Capabilities.*
import dotty.tools.dotc.cc.CaptureSet.MutAdaptFailure

/** The capture checker */
object CheckCaptures:
  import ast.tpd.*

  val name: String = "cc"
  val description: String = "capture checking"

  enum EnvKind:
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
  case class Env(
      owner: Symbol,
      kind: EnvKind,
      captured: CaptureSet,
      outer0: Env | Null,
      nestedClosure: Symbol = NoSymbol):

    def outer = outer0.nn

    def isOutermost = outer0 == null

    def outersIterator: Iterator[Env] = new:
      private var cur = Env.this
      def hasNext = !cur.isOutermost
      def next(): Env =
        val res = cur
        cur = cur.outer
        res

    def ownerString(using Context): String =
      if owner.isAnonymousFunction then "enclosing function" else owner.show
  end Env

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

  /** A prototype that indicates selection with an immutable value */
  class PathSelectionProto(val sym: Symbol, val pt: Type)(using Context) extends WildcardSelectionProto

  /** Check that a @retains annotation only mentions references that can be tracked.
   *  This check is performed at Typer.
   */
  def checkWellformed(parent: Tree, ann: Tree)(using Context): Unit =
    def check(elem: Type): Unit = elem match
      case ref: TypeRef =>
        val refSym = ref.symbol
        if refSym.isType && !refSym.info.derivesFrom(defn.Caps_CapSet) then
          report.error(em"$elem is not a legal element of a capture set", ann.srcPos)
      case ref: CoreCapability =>
        if !ref.isTrackableRef && !ref.isCapRef then
          report.error(em"$elem cannot be tracked since it is not a parameter or local value", ann.srcPos)
      case ReachCapability(ref) =>
        check(ref)
        if ref.isCapRef then
          report.error(em"Cannot form a reach capability from `cap`", ann.srcPos)
      case ReadOnlyCapability(ref) =>
        check(ref)
      case tpe =>
        report.error(em"$elem: $tpe is not a legal element of a capture set", ann.srcPos)
    ann.retainedSet.retainedElementsRaw.foreach(check)

  /** Under the sealed policy, report an error if some part of `tp` contains the
   *  root capability in its capture set or if it refers to a type parameter that
   *  could possibly be instantiated with cap in a way that's visible at the type.
   */
  private def disallowRootCapabilitiesIn(tp: Type, upto: Symbol, what: String, have: String, addendum: String, pos: SrcPos)(using Context) =
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
              refs.disallowRootCapability(upto): () =>
                def part =
                  if t eq tp then ""
                  else
                    // Show in context of all enclosing traversed existential scopes.
                    def showInOpenedFreshBinders(mts: List[MethodType]): String = mts match
                      case Nil => i"the part $t of "
                      case mt :: mts1 =>
                        inNewExistentialScope(mt):
                          showInOpenedFreshBinders(mts1)
                    showInOpenedFreshBinders(openScopes.reverse)
                report.error(
                  em"""$what cannot $have $tp since
                      |${part}that type captures the root capability `cap`.$addendum""",
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
  end disallowRootCapabilitiesIn

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
       *  that contains cap in no-flip covariant position, which will necessite
       *  a separation check?
       */
      def needsSepCheck: Boolean

      /** If a tree is an argument for which needsSepCheck is true,
       *  the type of the formal paremeter corresponding to the argument.
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

  def newRechecker()(using Context) = CaptureChecker(ctx)

  override def run(using Context): Unit =
    if Feature.ccEnabled then
      super.run

  val ccState1 = new CCState // Dotty problem: Rename to ccState ==> Crash in ExplicitOuter

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

    /** The references used at identifier or application trees */
    private val usedSet = util.EqHashMap[Tree, CaptureSet]()

    /** The set of symbols that were rechecked via a completer */
    private val completed = new mutable.HashSet[Symbol]

    var needAnotherRun = false

    def resetIteration()(using Context): Unit =
      needAnotherRun = false
      resetNuTypes()
      todoAtPostCheck.clear()
      completed.clear()

    extension [T <: Tree](tree: T)
      def needsSepCheck: Boolean = sepCheckFormals.contains(tree)
      def formalType: Type = sepCheckFormals.getOrElse(tree, NoType)
      def markedFree = usedSet.getOrElse(tree, CaptureSet.empty)

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

    /*  Also set any previously unset owners of toplevel Fresh instances to improve
     *  error diagnostics in separation checking.
     */
    private def anchorCaps(sym: Symbol)(using Context) = new TypeTraverser:
      override def traverse(t: Type) =
        if variance > 0 then
          t match
            case t @ CapturingType(parent, refs) =>
              for ref <- refs.elems do
                ref match
                  case ref: FreshCap if !ref.hiddenSet.givenOwner.exists =>
                    ref.hiddenSet.givenOwner = sym
                  case _ =>
              traverse(parent)
            case t @ defn.RefinedFunctionOf(rinfo) =>
              traverse(rinfo)
            case _ =>
              traverseChildren(t)

    /** If `tpt` is an inferred type, interpolate capture set variables appearing contra-
     *  variantly in it. Also anchor Fresh instances with anchorCaps.
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
    def checkOK(res: TypeComparer.CompareResult, prefix: => String, added: Capability | CaptureSet, target: CaptureSet, pos: SrcPos, provenance: => String = "")(using Context): Unit =
      res match
        case TypeComparer.CompareResult.Fail(notes) =>
          val ((res: IncludeFailure) :: Nil, otherNotes) =
            notes.partition(_.isInstanceOf[IncludeFailure]): @unchecked
          def msg(provisional: Boolean) =
            def toAdd: String = errorNotes(otherNotes).toAdd.mkString
            def descr: String =
              val d = res.cs.description
              if d.isEmpty then provenance else ""
            def kind = if provisional then "previously estimated\n" else "allowed "
            em"$prefix included in the ${kind}capture set ${res.cs}$descr$toAdd"
          target match
            case target: CaptureSet.Var
            if res.cs.isProvisionallySolved =>
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
          if cs1.elems.size == 1 then i"reference ${cs1.elems.nth(0)}$cs1description is not"
          else i"references $cs1$cs1description are not all",
          cs1, cs2, pos, provenance)

    /** If `sym` is a class or method nested inside a term, a capture set variable representing
     *  the captured variables of the environment associated with `sym`.
     */
    def capturedVars(sym: Symbol)(using Context): CaptureSet =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm)
        then CaptureSet.Var(sym.owner, level = ccState.symLevel(sym))
        else CaptureSet.empty)

// ---- Record Uses with MarkFree ----------------------------------------------------

    /** The next environment enclosing `env` that needs to be charged
     *  with free references.
     *  @param included Whether an environment is included in the range of
     *                  environments to charge. Once `included` is false, no
     *                  more environments need to be charged.
     */
    def nextEnvToCharge(env: Env, included: Env => Boolean)(using Context): Env =
      if env.owner.isConstructor && included(env.outer) then env.outer.outer
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

    /** Does the given environment belong to a method that is (a) nested in a term
     *  and (b) not the method of an anonymous function?
     */
    def isOfNestedMethod(env: Env | Null)(using Context) =
      env != null
      && env.owner.is(Method)
      && env.owner.owner.isTerm
      && !env.owner.isAnonymousFunction

    /** Include `sym` in the capture sets of all enclosing environments nested in the
     *  the environment in which `sym` is defined.
     */
    def markFree(sym: Symbol, tree: Tree)(using Context): Unit =
      markFree(sym, sym.termRef, tree)

    def markFree(sym: Symbol, ref: Capability, tree: Tree)(using Context): Unit =
      if sym.exists && ref.isTracked then markFree(ref.singletonCaptureSet, tree)

    /** Make sure the (projected) `cs` is a subset of the capture sets of all enclosing
     *  environments. At each stage, only include references from `cs` that are outside
     *  the environment's owner
     */
    def markFree(cs: CaptureSet, tree: Tree)(using Context): Unit =
      // A captured reference with the symbol `sym` is visible from the environment
      // if `sym` is not defined inside the owner of the environment.
      inline def isVisibleFromEnv(sym: Symbol, env: Env) =
        sym.exists && {
          if env.kind == EnvKind.NestedInOwner then
            !sym.isProperlyContainedIn(env.owner)
          else
            !sym.isContainedIn(env.owner)
        }

      /** If capability `c` refers to a parameter that is not @use declared, report an error.
       *  Exception under deferredReaches: If use comes from a nested closure, accept it.
       */
      def checkUseDeclared(c: Capability, env: Env, lastEnv: Env | Null) =
        if lastEnv != null && env.nestedClosure.exists && env.nestedClosure == lastEnv.owner then
          assert(ccConfig.deferredReaches) // access is from a nested closure under deferredReaches, so it's OK
        else c.paramPathRoot match
          case ref: NamedType if !ref.symbol.isUseParam =>
            val what = if ref.isType then "Capture set parameter" else "Local reach capability"
            report.error(
              em"""$what $c leaks into capture scope of ${env.ownerString}.
                  |To allow this, the ${ref.symbol} should be declared with a @use annotation""", tree.srcPos)
          case _ =>

      /** Avoid locally defined capability by charging the underlying type
       *  (which may not be cap). This scheme applies only under the deferredReaches setting.
       */
      def avoidLocalCapability(c: Capability, env: Env, lastEnv: Env | Null): Unit =
        if c.isParamPath then
          c match
            case Reach(_) | _: TypeRef =>
              checkUseDeclared(c, env, lastEnv)
            case _ =>
        else
          val underlying = c match
            case Reach(c1) => CaptureSet.ofTypeDeeply(c1.widen)
            case _ => c.core match
              case c1: RootCapability => c1.singletonCaptureSet
              case c1: CoreCapability =>
                CaptureSet.ofType(c1.widen, followResult = false)
          capt.println(i"Widen reach $c to $underlying in ${env.owner}")
          underlying.disallowRootCapability(NoSymbol): () =>
            report.error(em"Local capability $c in ${env.ownerString} cannot have `cap` as underlying capture set", tree.srcPos)
          recur(underlying, env, lastEnv)

      /** Avoid locally defined capability if it is a reach capability or capture set
       *  parameter. This is the default.
       */
      def avoidLocalReachCapability(c: Capability, env: Env): Unit = c match
        case Reach(c1) =>
          if c1.isParamPath then
            checkUseDeclared(c, env, null)
          else
            // When a reach capabilty x* where `x` is not a parameter goes out
            // of scope, we need to continue with `x`'s underlying deep capture set.
            // It is an error if that set contains cap.
            // The same is not an issue for normal capabilities since in a local
            // definition `val x = e`, the capabilities of `e` have already been charged.
            // Note: It's not true that the underlying capture set of a reach capability
            // is always cap. Reach capabilities over paths depend on the prefix, which
            // might turn a cap into something else.
            // The path-use.scala neg test contains an example.
            val underlying = CaptureSet.ofTypeDeeply(c1.widen)
            capt.println(i"Widen reach $c to $underlying in ${env.owner}")
            if ccConfig.useSepChecks then
              recur(underlying.filter(!_.isTerminalCapability), env, null)
                // we don't want to disallow underlying Fresh instances, since these are typically locally created
                // fresh capabilities. We don't need to also follow the hidden set since separation
                // checking makes ure that locally hidden references need to go to @consume parameters.
            else
              underlying.disallowRootCapability(ctx.owner): () =>
                report.error(em"Local reach capability $c leaks into capture scope of ${env.ownerString}", tree.srcPos)
              recur(underlying, env, null)
        case c: TypeRef if c.isParamPath =>
          checkUseDeclared(c, env, null)
        case _ =>

      def recur(cs: CaptureSet, env: Env, lastEnv: Env | Null): Unit =
        if env.kind != EnvKind.Boxed && !env.owner.isStaticOwner && !cs.isAlwaysEmpty then
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
            recur(included, nextEnvToCharge(env, !_.owner.isStaticOwner), env)
          // Don't propagate out of methods inside terms. The use set of these methods
          // will be charged when that method is called.

      recur(cs, curEnv, null)
      usedSet(tree) = tree.markedFree ++ cs
    end markFree

    /** Include references captured by the called method in the current environment stack */
    def includeCallCaptures(sym: Symbol, resType: Type, tree: Tree)(using Context): Unit = resType match
      case _: MethodOrPoly => // wait until method is fully applied
      case _ =>
        def isRetained(ref: Capability): Boolean = ref.pathRoot match
          case root: ThisType => ctx.owner.isContainedIn(root.cls)
          case _ => true
        if sym.exists && curEnv.kind != EnvKind.Boxed then
          markFree(capturedVars(sym).filter(isRetained), tree)

    /** If `tp` (possibly after widening singletons) is an ExprType
     *  of a parameterless method, map Result instances in it to Fresh instances
     */
    def mapResultRoots(tp: Type, sym: Symbol)(using Context): Type =
      tp.widenSingleton match
        case tp: ExprType if sym.is(Method) =>
          resultToFresh(tp, Origin.ResultInstance(tp, sym))
        case _ =>
          tp

    /** Under the sealed policy, disallow the root capability in type arguments.
     *  Type arguments come either from a TypeApply node or from an AppliedType
     *  which represents a trait parent in a template.
     *  Also, if a corresponding formal type parameter is declared or implied @use,
     *  charge the deep capture set of the argument to the environent.
     *  @param  fn   the type application, of type TypeApply or TypeTree
     *  @param  sym  the constructor symbol (could be a method or a val or a class)
     *  @param  args the type arguments
     */
    def disallowCapInTypeArgs(fn: Tree, sym: Symbol, args: List[Tree])(using Context): Unit =
      def isExempt = sym.isTypeTestOrCast || sym == defn.Compiletime_erasedValue
      if !isExempt then
        val paramNames = atPhase(thisPhase.prev):
          fn.tpe.widenDealias match
            case tl: TypeLambda => tl.paramNames
            case ref: AppliedType if ref.typeSymbol.isClass => ref.typeSymbol.typeParams.map(_.name)
            case t =>
              println(i"parent type: $t")
              args.map(_ => EmptyTypeName)

        for case (arg: TypeTree, pname) <- args.lazyZip(paramNames) do
          def where = if sym.exists then i" in an argument of $sym" else ""
          val (addendum, errTree) =
            if arg.isInferred
            then (i"\nThis is often caused by a local capability$where\nleaking as part of its result.", fn)
            else if arg.span.exists then ("", arg)
            else ("", fn)
          disallowRootCapabilitiesIn(arg.nuType, NoSymbol,
            i"Type variable $pname of $sym", "be instantiated to", addendum, errTree.srcPos)

          val param = fn.symbol.paramNamed(pname)
          if param.isUseParam then markFree(arg.nuType.deepCaptureSet, errTree)
    end disallowCapInTypeArgs

    /** Rechecking idents involves:
     *   - adding call captures for idents referring to methods
     *   - marking as free the identifier with any selections or .rd
     *     modifiers implied by the expected type
     */
    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      val sym = tree.symbol
      if sym.is(Method) then
        // If ident refers to a parameterless method, charge its cv to the environment
        includeCallCaptures(sym, sym.info, tree)
      else if !sym.isStatic then
        // Otherwise charge its symbol, but add all selections and also any `.rd`
        // modifier implied by the expected type `pt`.
        // Example: If we have `x` and the expected type says we select that with `.a.b`
        // where `b` is a read-only method, we charge `x.a.b.rd` instead of `x`.
        def addSelects(ref: TermRef, pt: Type): Capability = pt match
          case pt: PathSelectionProto if ref.isTracked =>
            if pt.sym.isReadOnlyMethod then
              ref.readOnly
            else
              // if `ref` is not tracked then the selection could not give anything new
              // class SerializationProxy in stdlib-cc/../LazyListIterable.scala has an example where this matters.
              addSelects(ref.select(pt.sym).asInstanceOf[TermRef], pt.pt)
          case _ => ref
        var pathRef: Capability = addSelects(sym.termRef, pt)
        if pathRef.derivesFromMutable && pt.isValueType && !pt.isMutableType then
          pathRef = pathRef.readOnly
        markFree(sym, pathRef, tree)
      mapResultRoots(super.recheckIdent(tree, pt), tree.symbol)

    /** The expected type for the qualifier of a selection. If the selection
     *  could be part of a capability path or is a a read-only method, we return
     *  a PathSelectionProto.
     */
    override def selectionProto(tree: Select, pt: Type)(using Context): Type =
      val sym = tree.symbol
      if !sym.isOneOf(UnstableValueFlags) && !sym.isStatic
          || sym.isReadOnlyMethod
      then PathSelectionProto(sym, pt)
      else super.selectionProto(tree, pt)

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
      // an exclusive reference. TODO This should probably rolled into
      // qualifier logic once we have it.
      if tree.symbol.isUpdateMethod && !qualType.captureSet.isExclusive then
        report.error(
            em"""cannot call update ${tree.symbol} from $qualType,
                |since its capture set ${qualType.captureSet} is read-only""",
            tree.srcPos)

      val selType = mapResultRoots(recheckSelection(tree, qualType, name, disambiguate), tree.symbol)
      val selWiden = selType.widen

      // Don't apply the rule
      //   - on the LHS of assignments, or
      //   - if the qualifier or selection type is boxed, or
      //   - the selection is either a trackable capture reference or a pure type
      if noWiden(selType, pt)
          || qualType.isBoxedCapturing
          || selType.isBoxedCapturing
          || selWiden.isBoxedCapturing
          || selType.isTrackableRef
          || selWiden.captureSet.isAlwaysEmpty
      then
        selType
      else
        val qualCs = qualType.captureSet
        val selCs = selType.captureSet
        capt.println(i"pick one of $qualType, ${selType.widen}, $qualCs, $selCs ${selWiden.captureSet} in $tree")

        if qualCs.mightSubcapture(selCs)
            //&& !selCs.mightSubcapture(qualCs)
            && !pt.stripCapturing.isInstanceOf[SingletonType]
        then
          selWiden.stripCapturing.capturing(qualCs)
            .showing(i"alternate type for select $tree: $selType --> $result, $qualCs / $selCs", capt)
        else
          selType
    }//.showing(i"recheck sel $tree, $qualType = $result")

    /** Recheck applications, with special handling of unsafeAssumePure.
     *  More work is done in `recheckApplication`, `recheckArg` and `instantiate` below.
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val meth = tree.fun.symbol
      if meth == defn.Caps_unsafeAssumePure then
        val arg :: Nil = tree.args: @unchecked
        val argType0 = recheck(arg, pt.stripCapturing.capturing(FreshCap(Origin.UnsafeAssumePure)))
        val argType =
          if argType0.captureSet.isAlwaysEmpty then argType0
          else argType0.widen.stripCapturing
        capt.println(i"rechecking unsafeAssumePure of $arg with $pt: $argType")
        super.recheckFinish(argType, tree, pt)
      else
        val res = super.recheckApply(tree, pt)
        includeCallCaptures(meth, res, tree)
        res

    /** Recheck argument against a "freshened" version of `formal` where toplevel `cap`
     *  occurrences are replaced by `Fresh` instances. Also, if formal parameter carries a `@use`,
     *  charge the deep capture set of the actual argument to the environment.
     */
    protected override def recheckArg(arg: Tree, formal: Type, pref: ParamRef, app: Apply)(using Context): Type =
      val freshenedFormal = capToFresh(formal, Origin.Formal(pref, app))
      val argType = recheck(arg, freshenedFormal)
        .showing(i"recheck arg $arg vs $freshenedFormal = $result", capt)
      if formal.hasAnnotation(defn.UseAnnot) || formal.hasAnnotation(defn.ConsumeAnnot) then
        // The @use and/or @consume annotation is added to `formal` when creating methods types.
        // See [[MethodTypeCompanion.adaptParamInfo]].
        capt.println(i"charging deep capture set of $arg: ${argType} = ${argType.deepCaptureSet}")
        markFree(argType.deepCaptureSet, arg)
      if formal.containsCap then
        sepCheckFormals(arg) = freshenedFormal
      argType

    /** Map existential captures in result to `cap` and implement the following
     *  rele:
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
      val appType = resultToFresh(
        super.recheckApplication(tree, qualType, funType, argTypes),
        Origin.ResultInstance(funType, tree.symbol))
      val qualCaptures = qualType.captureSet
      val argCaptures =
        for (argType, formal) <- argTypes.lazyZip(funType.paramInfos) yield
          if formal.hasAnnotation(defn.UseAnnot) then argType.deepCaptureSet else argType.captureSet
      appType match
        case appType @ CapturingType(appType1, refs)
        if qualType.exists
            && !tree.fun.symbol.isConstructor
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
       *  Second half: union of initial capture set and all capture sets of arguments
       *  to tracked parameters. The initial capture set `initCs` is augmented with
       *   - FreshCap(...)    if `core` extends Mutable
       *   - FreshCap(...).rd if `core` extends Capability
       */
      def addParamArgRefinements(core: Type, initCs: CaptureSet): (Type, CaptureSet) =
        var refined: Type = core
        var allCaptures: CaptureSet =
          if core.derivesFromMutable then
            initCs ++ FreshCap(Origin.NewMutable(core)).singletonCaptureSet
          else if core.derivesFromCapability then
            initCs ++ FreshCap(Origin.NewCapability(core)).readOnly.singletonCaptureSet
          else initCs
        for (getterName, argType) <- mt.paramNames.lazyZip(argTypes) do
          val getter = cls.info.member(getterName).suchThat(_.isRefiningParamAccessor).symbol
          if !getter.is(Private) && getter.hasTrackedParts then
            refined = refined.refinedOverride(getterName, argType.unboxed) // Yichen you might want to check this
            allCaptures ++= argType.captureSet
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

      augmentConstructorType(resType, capturedVars(cls) ++ capturedVars(constr))
        .showing(i"constr type $mt with $argTypes%, % in $constr = $result", capt)
    end refineConstructorInstance

    /** Recheck type applications:
     *   - Map existential captures in result to `cap`
     *   - include captures of called methods in environment
     *   - don't allow cap to appear covariantly in type arguments
     *   - special handling of `contains[A, B]` calls
     */
    override def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      val meth = tree.fun match
        case fun @ Select(qual, nme.apply) => qual.symbol.orElse(fun.symbol)
        case fun => fun.symbol
      def methDescr = if meth.exists then i"$meth's type " else ""
      disallowCapInTypeArgs(tree.fun, meth, tree.args)
      val funType = super.recheckTypeApply(tree, pt)
      val res = resultToFresh(funType, Origin.ResultInstance(funType, meth))
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

      def matchParams(paramss: List[ParamClause], pt: Type): Unit =
        //println(i"match $mdef against $pt")
        paramss match
        case params :: paramss1 => pt match
          case defn.PolyFunctionOf(poly: PolyType) =>
            assert(params.hasSameLengthAs(poly.paramInfos))
            matchParams(paramss1, poly.instantiate(params.map(_.symbol.typeRef)))
          case FunctionOrMethod(argTypes, resType) =>
            assert(params.hasSameLengthAs(argTypes), i"$mdef vs $pt, ${params}")
            for (argType, param) <- argTypes.lazyZip(params) do
              val paramTpt = param.asInstanceOf[ValDef].tpt
              val paramType = freshToCap(paramTpt.nuType)
              checkConformsExpr(argType, paramType, param)
                .showing(i"compared expected closure formal $argType against $param with ${paramTpt.nuType}", capt)
            if ccConfig.preTypeClosureResults && !(isEtaExpansion(mdef) && ccConfig.handleEtaExpansionsSpecially) then
              // Check whether the closure's result conforms to the expected type
              // This constrains parameter types of the closure which can give better
              // error messages.
              // But if the closure is an eta expanded method reference it's better to not constrain
              // its internals early since that would give error messages in generated code
              // which are less intelligible. An example is the line `a = x` in
              // neg-custom-args/captures/vars.scala. That's why this code is conditioned.
              // to apply only to closures that are not eta expansions.
              assert(paramss1.isEmpty)
              val respt0 = pt match
                case defn.RefinedFunctionOf(rinfo) =>
                  val paramTypes = params.map(_.asInstanceOf[ValDef].tpt.nuType)
                  rinfo.instantiate(paramTypes)
                case _ =>
                  resType
              val respt = resultToFresh(respt0, Origin.LambdaExpected(respt0))
              val res = resultToFresh(mdef.tpt.nuType, Origin.LambdaActual(mdef.tpt.nuType))
              // We need to open existentials here in order not to get vars mixed up in them
              // We do the proper check with existentials when we are finished with the closure block.
              capt.println(i"pre-check closure $expr of type $res against $respt")
              checkConformsExpr(res, respt, expr)
          case _ =>
        case Nil =>

      openClosures = (mdef.symbol, pt) :: openClosures
        // openClosures is needed for errors but currently makes no difference
        // TODO follow up on this
      try
        matchParams(mdef.paramss, pt)
        capt.println(i"recheck closure block $mdef: ${mdef.symbol.infoOrCompleter}")
        if !mdef.symbol.isCompleted then
          mdef.symbol.ensureCompleted() // this will recheck def
        else
          recheckDef(mdef, mdef.symbol)

        recheckClosure(expr, pt, forceDependent = true)
      finally
        openClosures = openClosures.tail
    end recheckClosureBlock

    /** Elements of a SeqLiteral instantiate a Seq or Array parameter, so they
     *  should be boxed.
     */
    override def seqLiteralElemProto(tree: SeqLiteral, pt: Type, declared: Type)(using Context) =
      super.seqLiteralElemProto(tree, pt, declared).boxed

    /** Recheck val and var definitions:
     *   - disallow cap in the type of mutable vars.
     *   - for externally visible definitions: check that their inferred type
     *     does not refine what was known before capture checking.
     *   - Interpolate contravariant capture set variables in result type.
     */
    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      try
        if sym.is(Module) then sym.info // Modules are checked by checking the module class
        else
          if sym.is(Mutable) && !sym.hasAnnotation(defn.UncheckedCapturesAnnot) then
            val addendum = capturedBy.get(sym) match
              case Some(encl) =>
                val enclStr =
                  if encl.isAnonymousFunction then
                    val location = anonFunCallee.get(encl) match
                      case Some(meth) if meth.exists => i" argument in a call to $meth"
                      case _ => ""
                    s"an anonymous function$location"
                  else encl.show
                i"\n\nNote that $sym does not count as local since it is captured by $enclStr"
              case _ =>
                ""
            disallowRootCapabilitiesIn(
              tree.tpt.nuType, NoSymbol, i"Mutable $sym", "have type", addendum, sym.srcPos)
          checkInferredResult(super.recheckValDef(tree, sym), tree)
      finally
        if !sym.is(Param) then
          // Parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateIfInferred(tree.tpt, sym)

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
      else
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

        ccState.inNestedLevel: // TODO: nestedLevel needed here?
          try checkInferredResult(super.recheckDefDef(tree, sym)(using bodyCtx), tree)
          finally
            if !sym.isAnonymousFunction then
              // Anonymous functions propagate their type to the enclosing environment
              // so it is not in general sound to interpolate their types.
              interpolateIfInferred(tree.tpt, sym)
            curEnv = saved
    end recheckDefDef

    /** If val or def definition with inferred (result) type is visible
     *  in other compilation units, check that the actual inferred type
     *  conforms to the expected type where all inferred capture sets are dropped.
     *  This ensures that if files compile separately, they will also compile
     *  in a joint compilation.
     */
    def checkInferredResult(tp: Type, tree: ValOrDefDef)(using Context): Type =
      val sym = tree.symbol

      def canUseInferred =    // If canUseInferred is false, all capturing types in the type of `sym` need to be given explicitly
        sym.isLocalToCompilationUnit      // Symbols that can't be seen outside the compilation unit can always have inferred types
        || ctx.owner.enclosingPackageClass.isEmptyPackage
                                          // We make an exception for symbols in the empty package.
                                          // these could theoretically be accessed from other files in the empty package, but
                                          // usually it would be too annoying to require explicit types.
        || sym.name.is(DefaultGetterName) // Default getters are exempted since otherwise it would be
                                          // too annoying. This is a hole since a defualt getter's result type
                                          // might leak into a type variable.

      def fail(tree: Tree, expected: Type, addenda: Addenda): Unit =
        def maybeResult = if sym.is(Method) then " result" else ""
        report.error(
          em"""$sym needs an explicit$maybeResult type because the inferred type does not conform to
              |the type that is externally visible in other compilation units.
              |
              | Inferred type          : ${tree.tpe}
              | Externally visible type: $expected""",
          tree.srcPos)

      def addenda(expected: Type) = new Addenda:
        override def toAdd(using Context) =
          def result = if tree.isInstanceOf[ValDef] then"" else " result"
          i"""
           |
           |Note that the expected type $expected
           |is the previously inferred$result type of $sym
           |which is also the type seen in separately compiled sources.
           |The new inferred type $tp
           |must conform to this type.""" :: Nil

      tree.tpt match
        case tpt: InferredTypeTree if !canUseInferred =>
          val expected = tpt.tpe.dropAllRetains
          todoAtPostCheck += { () =>
            withCapAsRoot:
              testAdapted(tp, expected, tree.rhs, addenda(expected))(fail)
              // The check that inferred <: expected is done after recheck so that it
              // does not interfere with normal rechecking by constraining capture set variables.
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
          val localSet = capturedVars(sym)
          if localSet eq CaptureSet.empty then rootEnv
          else envForOwner.get(sym) match
            case Some(e) => e
            case None => Env(sym, EnvKind.Regular, localSet, restoreEnvFor(sym.owner))
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
     *   3. The capture set of the self type of a class includes the capture set of every class parameter,
     *      unless the parameter is marked @constructorOnly or @untrackedCaptures.
     *   4. If the class extends a pure base class, the capture set of the self type must be empty.
     *  Also, check that trait parents represented as applied types don't have cap in their
     *  type arguments. Other generic parents are represented as TypeApplys, where the same check
     *  is already done in the TypeApply.
     */
    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      val localSet = capturedVars(cls)
      for parent <- impl.parents do // (1)
        checkSubset(capturedVars(parent.tpe.classSymbol), localSet, parent.srcPos,
          i"\nof the references allowed to be captured by $cls")
      val saved = curEnv
      curEnv = Env(cls, EnvKind.Regular, localSet, curEnv)
      try
        val thisSet = cls.classInfo.selfType.captureSet.withDescription(i"of the self type of $cls")
        checkSubset(localSet, thisSet, tree.srcPos) // (2)
        for param <- cls.paramGetters do
          if !param.hasAnnotation(defn.ConstructorOnlyAnnot)
              && !param.hasAnnotation(defn.UntrackedCapturesAnnot) then
            withCapAsRoot: // OK? We need this here since self types use `cap` instead of `fresh`
              checkSubset(param.termRef.captureSet, thisSet, param.srcPos) // (3)
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
        for case tpt: TypeTree <- impl.parents do
          tpt.tpe match
            case AppliedType(fn, args) =>
              disallowCapInTypeArgs(tpt, fn.typeSymbol, args.map(TypeTree(_)))
            case _ =>
        ccState.inNestedLevelUnless(cls.is(Module)):
          super.recheckClassDef(tree, impl, cls)
      finally
        completed += cls
        curEnv = saved

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

    /** Under the sealed policy and with saferExceptions, disallow cap in the
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
        disallowRootCapabilitiesIn(tp, ctx.owner,
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
            CaptureSet.Var(curEnv.owner, level = ccState.currentLevel), curEnv)
        case _ =>
      val res =
        try
          if capt eq noPrinter then
            super.recheck(tree, pt)
          else
            trace.force(i"rechecking $tree with pt = $pt", recheckr, show = true):
              super.recheck(tree, pt)
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
    //   - Instantiate covariant occurrenves of `cap` in actual to reach capabilities.

    private inline val debugSuccesses = false

    type BoxErrors = mutable.ListBuffer[Message] | Null

    private def errorNotes(notes: List[TypeComparer.ErrorNote])(using Context): Addenda =
      val printableNotes = notes.filter:
        case IncludeFailure(_, _, true) => true
        case _: ExistentialSubsumesFailure | _: MutAdaptFailure => true
        case _ => false
      if printableNotes.isEmpty then NothingToAdd
      else new Addenda:
        override def toAdd(using Context) = printableNotes.map: note =>
          val msg = note match
            case IncludeFailure(cs, ref, _) =>
              if ref.core.isCapOrFresh then
                i"""the universal capability $ref
                   |cannot be included in capture set $cs"""
              else
                val levelStr = ref match
                  case ref: TermRef => i", defined in ${ref.symbol.maybeOwner}"
                  case _ => ""
                i"""reference ${ref}$levelStr
                    |cannot be included in outer capture set $cs"""
            case ExistentialSubsumesFailure(ex, other) =>
              def since =
                if other.isTerminalCapability then ""
                else " since that capability is not a SharedCapability"
              i"""the existential capture root in ${ex.originalBinder.resType}
                 |cannot subsume the capability $other$since"""
            case MutAdaptFailure(cs, lo, hi) =>
              def ofType(tp: Type) = if tp.exists then i"of the mutable type $tp" else "of a mutable type"
              i"""$cs is an exclusive capture set ${ofType(hi)},
                 |it cannot subsume a read-only capture set ${ofType(lo)}."""
          i"""
             |Note that ${msg.toString}"""


    /** Addendas for error messages that show where we have under-approximated by
     *  mapping a a capability in contravariant position to the empty set because
     *  the original result type of the map was not itself a capability.
     */
    private def addApproxAddenda(using Context) =
      new TypeAccumulator[Addenda]:
        def apply(add: Addenda, t: Type) = t match
          case CapturingType(t, CaptureSet.EmptyWithProvenance(ref, mapped)) =>
            /* val (origCore, kind) = original match
              case tp @ AnnotatedType(parent, ann) if ann.hasSymbol(defn.ReachCapabilityAnnot) =>
                (parent, " deep")
              case _ =>
                (original, "")*/
            add ++ new Addenda:
              override def toAdd(using Context): List[String] =
                i"""
                   |
                   |Note that a capability $ref in a capture set appearing in contravariant position
                   |was mapped to $mapped which is not a capability. Therefore, it was under-approximated to the empty set."""
                :: Nil
          case _ =>
            foldOver(add, t)

    /** Massage `actual` and `expected` types before checking conformance.
     *  Massaging is done by the methods following this one:
     *   - align dependent function types and add outer references in the expected type
     *   - adapt boxing in the actual type
     *  If the resulting types are not compatible, try again with an actual type
     *  where local capture roots are instantiated to root variables.
     */
    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree, addenda: Addenda)(using Context): Type =
      testAdapted(actual, expected, tree, addenda)(err.typeMismatch)

    inline def testAdapted(actual: Type, expected: Type, tree: Tree, addenda: Addenda)
        (fail: (Tree, Type, Addenda) => Unit)(using Context): Type =
      var expected1 = alignDependentFunction(expected, actual.stripCapturing)
      val falseDeps = expected1 ne expected
      val actualBoxed = adapt(actual, expected1, tree)
      //println(i"check conforms $actualBoxed <<< $expected1")

      if actualBoxed eq actual then
        // Only `addOuterRefs` when there is no box adaptation
        expected1 = addOuterRefs(expected1, actual, tree.srcPos)
      TypeComparer.compareResult(isCompatible(actualBoxed, expected1)) match
        case TypeComparer.CompareResult.Fail(notes) =>
          capt.println(i"conforms failed for ${tree}: $actual vs $expected")
          if falseDeps then expected1 = unalignFunction(expected1)
          fail(tree.withType(actualBoxed), expected1,
            addApproxAddenda(addenda ++ errorNotes(notes), expected1))
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
      @annotation.tailrec def walk(env: Env | Null): Unit =
        if env != null then
          sb ++= showEnv(env)
          sb ++= "\n"
          walk(env.outer0)
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
              curEnv.owner, EnvKind.NestedInOwner,
              CaptureSet.Var(curEnv.owner, level = ccState.currentLevel),
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

        def adaptStr = i"adapting $actual ${if covariant then "~~>" else "<~~"} $expected"

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
            if covariant then captures   // covariant: we box with captures of actual type plus captures leaked by inner adapation
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
          case widened @ CapturingType(p, refs) if ref.singletonCaptureSet.mightSubcapture(refs) =>
            val improvedCs =
              if widened.isBoxed then ref.reach.singletonCaptureSet
              else ref.singletonCaptureSet
            widened.derivedCapturingType(p, improvedCs)
              .showing(i"improve $widened to $result", capt)
          case _ => widened
      case _ => widened

    /** If actual is a capturing type T^C extending Mutable, and expected is an
     *  unboxed non-singleton value type not extending mutable, narrow the capture
     *  set `C` to `ro(C)`.
     *  The unboxed condition ensures that the expected type is not a type variable
     *  that's upper bounded by a read-only type. In this case it would not be sound
     *  to narrow to the read-only set, since that set can be propagated
     *  by the type variable instantiation.
     */
    private def improveReadOnly(actual: Type, expected: Type)(using Context): Type = reporting.trace(i"improv ro $actual vs $expected"):
      actual.dealiasKeepAnnots match
      case actual @ CapturingType(parent, refs) =>
        val parent1 = improveReadOnly(parent, expected)
        val refs1 =
          if parent1.derivesFrom(defn.Caps_Mutable)
              && expected.isValueType
              && (!expected.derivesFromMutable || expected.captureSet.isAlwaysReadOnly)
              && !expected.isSingleton
              && actual.isBoxedCapturing == expected.isBoxedCapturing
          then refs.readOnly
          else refs
        actual.derivedCapturingType(parent1, refs1)
      case actual @ FunctionOrMethod(aargs, ares) =>
        expected.dealias.stripCapturing match
          case FunctionOrMethod(eargs, eres) =>
            actual.derivedFunctionOrMethod(aargs, improveReadOnly(ares, eres))
          case _ =>
            actual
      case actual @ AppliedType(atycon, aargs) =>
        def improveArgs(aargs: List[Type], eargs: List[Type], formals: List[ParamInfo]): List[Type] =
          aargs match
            case aargs @ (aarg :: aargs1) =>
              val aarg1 =
                if formals.head.paramVariance.is(Covariant)
                then improveReadOnly(aarg, eargs.head)
                else aarg
              aargs.derivedCons(aarg1, improveArgs(aargs1, eargs.tail, formals.tail))
            case Nil =>
              aargs
        val expected1 = expected.dealias.stripCapturing
        val esym = expected1.typeSymbol
        expected1 match
          case AppliedType(etycon, eargs) =>
            if atycon.typeSymbol == esym then
              actual.derivedAppliedType(atycon,
                improveArgs(aargs, eargs, etycon.typeParams))
            else if esym.isClass then
              // This case is tricky: Try to lift actual to the base type with class `esym`,
              // improve the resulting arguments, and figure out if anything can be
              // deduced from that for the original arguments.
              actual.baseType(esym) match
                case base @ AppliedType(_, bargs) =>
                  // If any of the base type arguments can be improved, check
                  // whether they are the same as an original argument, and in this
                  // case improve the original argument.
                  val iargs = improveArgs(bargs, eargs, etycon.typeParams)
                  if iargs ne bargs then
                    val updates =
                      for
                        (barg, iarg) <- bargs.lazyZip(iargs)
                        if barg ne iarg
                        aarg <- aargs.find(_ eq barg)
                      yield (aarg, iarg)
                    if updates.nonEmpty then AppliedType(atycon, aargs.map(updates.toMap))
                    else actual
                  else actual
                case _ => actual
            else actual
          case _ =>
            actual
      case actual @ RefinedType(aparent, aname, ainfo) =>
        expected.dealias.stripCapturing match
          case RefinedType(eparent, ename, einfo) if aname == ename =>
            actual.derivedRefinedType(
              improveReadOnly(aparent, eparent),
              aname,
              improveReadOnly(ainfo, einfo))
          case _ =>
            actual
      case actual @ AnnotatedType(parent, ann) =>
        actual.derivedAnnotatedType(improveReadOnly(parent, expected), ann)
      case _ =>
        actual
    end improveReadOnly

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
          case expected: PathSelectionProto => !expected.sym.isOneOf(UnstableValueFlags)
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
        val improved = improveReadOnly(improvedVAR, expected)
        val adapted = adaptBoxed(
            improved.withReachCaptures(actual), expected, tree,
            covariant = true, alwaysConst = false)
        if adapted eq improvedVAR // no .rd improvement, no box-adaptation
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
                                CaptureSet.fresh(Origin.OverriddenType(member))))))
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

        /** Check that overrides don't change the @use or @consume status of their parameters */
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
      end OverridingPairsCheckerCC

      def traverse(t: Tree)(using Context) =
        t match
          case t: Template =>
            withCollapsedFresh:
              checkAllOverrides(ctx.owner.asClass, OverridingPairsCheckerCC(_, _, t))
          case _ =>
        traverseChildren(t)
    end checkOverrides

    /** Used for error reporting:
     *  Maps mutable variables to the symbols that capture them (in the
     *  CheckCaptures sense, i.e. symbol is referred to from a different method
     *  than the one it is defined in).
     */
    private val capturedBy = util.HashMap[Symbol, Symbol]()

    /** Used for error reporting:
     *  Maps anonymous functions appearing as function arguments to
     *  the function that is called.
     */
    private val anonFunCallee = util.HashMap[Symbol, Symbol]()

    /** Used for error reporting:
     *  Populates `capturedBy` and `anonFunCallee`. Called by `checkUnit`.
     */
    private def collectCapturedMutVars(using Context) = new TreeTraverser:
      def traverse(tree: Tree)(using Context) = tree match
        case id: Ident =>
          val sym = id.symbol
          if sym.isMutableVar && sym.owner.isTerm then
            val enclMeth = ctx.owner.enclosingMethod
            if sym.enclosingMethod != enclMeth then
              capturedBy(sym) = enclMeth
        case Apply(fn, args) =>
          for case closureDef(mdef) <- args do
            anonFunCallee(mdef.symbol) = fn.symbol
          traverseChildren(tree)
        case Inlined(_, bindings, expansion) =>
          traverse(bindings)
          traverse(expansion)
        case mdef: DefDef =>
          if !mdef.symbol.isInlineMethod then traverseChildren(tree)
        case _ =>
          traverseChildren(tree)

    private val setup: SetupAPI = thisPhase.prev.asInstanceOf[Setup]

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      ccState.start()
      setup.setupUnit(unit.tpdTree, this)
      collectCapturedMutVars.traverse(unit.tpdTree)

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
     *  without an explicit self type have the universal capture set `{cap}` on the
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

    /** Under the unsealed policy: Arrays are like vars, check that their element types
     *  do not contains `cap` (in fact it would work also to check on array creation
     *  like we do under sealed).
     */
    def checkArraysAreSealedIn(tp: Type, pos: SrcPos)(using Context): Unit =
      val check = new TypeTraverser:
        def traverse(t: Type): Unit =
          t match
            case AppliedType(tycon, arg :: Nil) if tycon.typeSymbol == defn.ArrayClass =>
              if !(pos.span.isSynthetic && ctx.reporter.errorsReported)
                && !arg.typeSymbol.name.is(WildcardParamName)
              then
                CheckCaptures.disallowRootCapabilitiesIn(arg, NoSymbol,
                  "Array", "have element type", "",
                  pos)
              traverseChildren(t)
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              traverse(rinfo)
            case _ =>
              traverseChildren(t)
      check.traverse(tp)

    /** Check that arguments of TypeApplys and AppliedTypes conform to their bounds.
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
                withCollapsedFresh: // OK? We need this since bounds use `cap` instead of `fresh`
                  checkBounds(normArgs, tl)
                if ccConfig.postCheckCapturesets then
                  args.lazyZip(tl.paramNames).foreach(checkTypeParam(_, _, fun.symbol))
              case _ =>
          case _ =>
        end check
      end checker

      checker.traverse(unit)(using ctx.withOwner(defn.RootClass))
      if ccConfig.useSepChecks then SepCheck(this).traverse(unit)
      if !ctx.reporter.errorsReported then
        // We dont report errors here if previous errors were reported, because other
        // errors often result in bad applied types, but flagging these bad types gives
        // often worse error messages than the original errors.
        val checkApplied = new TreeTraverser:
          def traverse(t: Tree)(using Context) = t match
            case tree: InferredTypeTree =>
            case tree: New =>
            case tree: TypeTree =>
              withCollapsedFresh:
                checkAppliedTypesIn(tree.withType(tree.nuType))
            case _ => traverseChildren(t)
        checkApplied.traverse(unit)
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
