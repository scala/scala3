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
import CaptureSet.{withCaptureSetsExplained, IdempotentCaptRefMap, CompareResult}
import CCState.*
import StdNames.nme
import NameKinds.{DefaultGetterName, WildcardParamName, UniqueNameKind}
import reporting.{trace, Message, OverrideError}

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
   *  @param owner     the current owner
   *  @param kind      the environment's kind
   *  @param captured  the capture set containing all references to tracked free variables outside of boxes
   *  @param outer0    the next enclosing environment
   */
  case class Env(
      owner: Symbol,
      kind: EnvKind,
      captured: CaptureSet,
      outer0: Env | Null,
      nestedClosure: Symbol = NoSymbol):

    def outer = outer0.nn

    def isOutermost = outer0 == null

    /** If an environment is open it tracks free references */
    def isOpen = !captured.isAlwaysEmpty && kind != EnvKind.Boxed

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
  extends ApproximatingTypeMap, IdempotentCaptRefMap:
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
  end SubstParamsMap

  final class SubstParamsBiMap(from: LambdaType, to: List[Type])(using Context)
  extends BiTypeMap:
    thisMap =>

    def apply(tp: Type): Type = tp match
      case tp: ParamRef =>
        if tp.binder == from then to(tp.paramNum) else tp
      case tp: NamedType =>
        if tp.prefix `eq` NoPrefix then tp
        else tp.derivedSelect(apply(tp.prefix))
      case _: ThisType =>
        tp
      case _ =>
        mapOver(tp)

    lazy val inverse = new BiTypeMap:
      def apply(tp: Type): Type = tp match
        case tp: NamedType =>
          var idx = 0
          var to1 = to
          while idx < to.length && (tp ne to(idx)) do
            idx += 1
            to1 = to1.tail
          if idx < to.length then from.paramRefs(idx)
          else if tp.prefix `eq` NoPrefix then tp
          else tp.derivedSelect(apply(tp.prefix))
        case _: ThisType =>
          tp
        case _ =>
          mapOver(tp)
      def inverse = thisMap
  end SubstParamsBiMap

  /** A prototype that indicates selection with an immutable value */
  class PathSelectionProto(val sym: Symbol, val pt: Type)(using Context) extends WildcardSelectionProto

  /** Check that a @retains annotation only mentions references that can be tracked.
   *  This check is performed at Typer.
   */
  def checkWellformed(parent: Tree, ann: Tree)(using Context): Unit =
    def check(elem: Tree, pos: SrcPos): Unit = elem.tpe match
      case ref: CaptureRef =>
        if !ref.isTrackableRef then
          report.error(em"$elem cannot be tracked since it is not a parameter or local value", pos)
      case tpe =>
        report.error(em"$elem: $tpe is not a legal element of a capture set", pos)
    for elem <- ann.retainedElems do
      elem match
        case CapsOfApply(arg) =>
          def isLegalCapsOfArg =
            arg.symbol.isType && arg.symbol.info.derivesFrom(defn.Caps_CapSet)
          if !isLegalCapsOfArg then
            report.error(
              em"""$arg is not a legal prefix for `^` here,
                  |is must be a type parameter or abstract type with a caps.CapSet upper bound.""",
              elem.srcPos)
        case ReachCapabilityApply(arg) => check(arg, elem.srcPos)
        case _ => check(elem, elem.srcPos)

  /** Under the sealed policy, report an error if some part of `tp` contains the
   *  root capability in its capture set or if it refers to a type parameter that
   *  could possibly be instantiated with cap in a way that's visible at the type.
   */
  private def disallowRootCapabilitiesIn(tp: Type, carrier: Symbol, what: String, have: String, addendum: String, pos: SrcPos)(using Context) =
    val check = new TypeTraverser:

      private val seen = new EqHashSet[TypeRef]

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
              refs.disallowRootCapability: () =>
                def part = if t eq tp then "" else i"the part $t of "
                report.error(
                  em"""$what cannot $have $tp since
                      |${part}that type captures the root capability `cap`.$addendum""",
                  pos)
            traverse(parent)
          case t =>
            traverseChildren(t)
    if ccConfig.useSealed then check.traverse(tp)
  end disallowRootCapabilitiesIn

  /** Under the sealed policy, disallow the root capability in type arguments.
   *  Type arguments come either from a TypeApply node or from an AppliedType
   *  which represents a trait parent in a template.
   *  @param  fn   the type application, of type TypeApply or TypeTree
   *  @param  sym  the constructor symbol (could be a method or a val or a class)
   *  @param  args the type arguments
   */
  private def disallowCapInTypeArgs(fn: Tree, sym: Symbol, args: List[Tree], thisPhase: Phase)(using Context): Unit =
    def isExempt = sym.isTypeTestOrCast || sym == defn.Compiletime_erasedValue
    if ccConfig.useSealed && !isExempt then
      val paramNames = atPhase(thisPhase.prev):
        fn.tpe.widenDealias match
          case tl: TypeLambda => tl.paramNames
          case ref: AppliedType if ref.typeSymbol.isClass => ref.typeSymbol.typeParams.map(_.name)
          case t =>
            println(i"parent type: $t")
            args.map(_ => EmptyTypeName)
      for case (arg: TypeTree, pname) <- args.lazyZip(paramNames) do
        def where = if sym.exists then i" in an argument of $sym" else ""
        val (addendum, pos) =
          if arg.isInferred
          then ("\nThis is often caused by a local capability$where\nleaking as part of its result.", fn.srcPos)
          else if arg.span.exists then ("", arg.srcPos)
          else ("", fn.srcPos)
        disallowRootCapabilitiesIn(arg.knownType, NoSymbol,
          i"Type variable $pname of $sym", "be instantiated to", addendum, pos)
  end disallowCapInTypeArgs

  /** If we are not under the sealed policy, and a tree is an application that unboxes
   *  its result or is a try, check that the tree's type does not have covariant universal
   *  capabilities.
   */
  private def checkNotUniversalInUnboxedResult(tpe: Type, tree: Tree)(using Context): Unit =
    def needsUniversalCheck = tree match
      case _: RefTree | _: Apply | _: TypeApply => tree.symbol.unboxesResult
      case _: Try => true
      case _ => false

    object checkNotUniversal extends TypeTraverser:
      def traverse(tp: Type) =
        tp.dealias match
        case wtp @ CapturingType(parent, refs) =>
          if variance > 0 then
            refs.disallowRootCapability: () =>
              def part = if wtp eq tpe.widen then "" else i" in its part $wtp"
              report.error(
                em"""The expression's type ${tpe.widen} is not allowed to capture the root capability `cap`$part.
                  |This usually means that a capability persists longer than its allowed lifetime.""",
                tree.srcPos)
          if !wtp.isBoxed then traverse(parent)
        case tp =>
          traverseChildren(tp)

    if !ccConfig.useSealed
        && !tpe.hasAnnotation(defn.UncheckedCapturesAnnot)
        && needsUniversalCheck
        && tpe.widen.isValueType
    then
      checkNotUniversal.traverse(tpe.widen)
  end checkNotUniversalInUnboxedResult

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

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):

    override def keepType(tree: Tree) =
      super.keepType(tree)
      || tree.isInstanceOf[Try]  // type of `try` needs tp be checked for * escapes

    /** Instantiate capture set variables appearing contra-variantly to their
     *  upper approximation.
     */
    private def interpolator(startingVariance: Int = 1)(using Context) = new TypeTraverser:
      variance = startingVariance
      override def traverse(t: Type) = t match
        case t @ CapturingType(parent, refs) =>
          refs match
            case refs: CaptureSet.Var if variance < 0 => refs.solve()
            case _ =>
          traverse(parent)
        case t @ defn.RefinedFunctionOf(rinfo) =>
          traverse(rinfo)
        case _ =>
          traverseChildren(t)

    /** If `tpt` is an inferred type, interpolate capture set variables appearing contra-
     *  variantly in it.
     */
    private def interpolateVarsIn(tpt: Tree)(using Context): Unit =
      if tpt.isInstanceOf[InferredTypeTree] then
        interpolator().traverse(tpt.knownType)
          .showing(i"solved vars in ${tpt.knownType}", capt)
      for msg <- ccState.approxWarnings do
        report.warning(msg, tpt.srcPos)
      ccState.approxWarnings.clear()

    /** Assert subcapturing `cs1 <: cs2` */
    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert(cs1.subCaptures(cs2, frozen = false).isOK, i"$cs1 is not a subset of $cs2")

    def checkOK(res: CompareResult, prefix: => String, pos: SrcPos, provenance: => String = "")(using Context): Unit =
      if !res.isOK then
        def toAdd: String = CaptureSet.levelErrors.toAdd.mkString
        def descr: String =
          val d = res.blocking.description
          if d.isEmpty then provenance else ""
        report.error(em"$prefix included in the allowed capture set ${res.blocking}$descr$toAdd", pos)

    /** Check subcapturing `{elem} <: cs`, report error on failure */
    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos, provenance: => String = "")(using Context) =
      checkOK(
          elem.singletonCaptureSet.subCaptures(cs, frozen = false),
          i"$elem cannot be referenced here; it is not",
          pos, provenance)

    /** Check subcapturing `cs1 <: cs2`, report error on failure */
    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos,
        provenance: => String = "", cs1description: String = "")(using Context) =
      checkOK(
          cs1.subCaptures(cs2, frozen = false),
          if cs1.elems.size == 1 then i"reference ${cs1.elems.toList.head}$cs1description is not"
          else i"references $cs1$cs1description are not all",
          pos, provenance)

    def showRef(ref: CaptureRef)(using Context): String =
        ctx.printer.toTextCaptureRef(ref).show

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
    private var todoAtPostCheck = new mutable.ListBuffer[() => Unit]

    /** If `sym` is a class or method nested inside a term, a capture set variable representing
     *  the captured variables of the environment associated with `sym`.
     */
    def capturedVars(sym: Symbol)(using Context): CaptureSet =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm)
        then CaptureSet.Var(sym.owner, level = sym.ccLevel)
        else CaptureSet.empty)

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

    def isOfNestedMethod(env: Env | Null)(using Context) =
      env != null
      && env.owner.is(Method)
      && env.owner.owner.isTerm
      && !env.owner.isAnonymousFunction

    /** Include `sym` in the capture sets of all enclosing environments nested in the
     *  the environment in which `sym` is defined.
     */
    def markFree(sym: Symbol, pos: SrcPos)(using Context): Unit =
      markFree(sym, sym.termRef, pos)

    def markFree(sym: Symbol, ref: TermRef, pos: SrcPos)(using Context): Unit =
      if sym.exists && ref.isTracked then markFree(ref.captureSet, pos)

    /** Make sure (projected) `cs` is a subset of the capture sets of all enclosing
     *  environments. At each stage, only include references from `cs` that are outside
     *  the environment's owner
     */
    def markFree(cs: CaptureSet, pos: SrcPos)(using Context): Unit =
      // A captured reference with the symbol `sym` is visible from the environment
      // if `sym` is not defined inside the owner of the environment.
      inline def isVisibleFromEnv(sym: Symbol, env: Env) =
        if env.kind == EnvKind.NestedInOwner then
          !sym.isProperlyContainedIn(env.owner)
        else
          !sym.isContainedIn(env.owner)

      def checkUseDeclared(c: CaptureRef, env: Env, lastEnv: Env | Null) =
        if lastEnv != null && env.nestedClosure.exists && env.nestedClosure == lastEnv.owner then
          () // access is from a nested closure, so it's OK
        else c.pathRoot match
          case ref: NamedType if !ref.symbol.hasAnnotation(defn.UseAnnot) =>
            val what = if ref.isType then "Capture set parameter" else "Local reach capability"
            report.error(
              em"""$what $c leaks into capture scope of ${env.ownerString}.
                  |To allow this, the ${ref.symbol} should be declared with a @use annotation""", pos)
          case _ =>

      def recur(cs: CaptureSet, env: Env, lastEnv: Env | Null)(using Context): Unit =
        if env.isOpen && !env.owner.isStaticOwner && !cs.isAlwaysEmpty then
          // Only captured references that are visible from the environment
          // should be included.
          val included = cs.filter: c =>
            val isVisible = c.pathRoot match
              case ref: NamedType => isVisibleFromEnv(ref.symbol.owner, env)
              case ref: ThisType => isVisibleFromEnv(ref.cls, env)
              case ref =>
                false
            if !isVisible then
              //println(i"out of scope: $c")
              if ccConfig.deferredReaches then // avoid all locally bound capabilities
                if c.isParamPath then
                  c match
                    case ReachCapability(_) | _: TypeRef =>
                      checkUseDeclared(c, env, lastEnv)
                    case _ =>
                else
                  val underlying = c match
                    case ReachCapability(c1) =>
                      CaptureSet.ofTypeDeeply(c1.widen)
                    case _ =>
                      CaptureSet.ofType(c.widen, followResult = false)
                    capt.println(i"Widen reach $c to $underlying in ${env.owner}")
                  underlying.disallowRootCapability: () =>
                    report.error(em"Local capability $c in ${env.ownerString} cannot have `cap` as underlying capture set", pos)
                  recur(underlying, env, lastEnv)
              else c match // avoid only reach capabilities and capture sets
                case ReachCapability(c1) =>
                  if c1.isParamPath then
                    checkUseDeclared(c, env, lastEnv)
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
                    underlying.disallowRootCapability: () =>
                      report.error(em"Local reach capability $c leaks into capture scope of ${env.ownerString}", pos)
                    recur(underlying, env, lastEnv)
                case c: TypeRef if c.isParamPath =>
                  checkUseDeclared(c, env, lastEnv)
                case _ =>
            isVisible
          checkSubset(included, env.captured, pos, provenance(env))
          capt.println(i"Include call or box capture $included from $cs in ${env.owner} --> ${env.captured}")
          if !isOfNestedMethod(env) then
            recur(included, nextEnvToCharge(env, !_.owner.isStaticOwner), env)
      recur(cs, curEnv, null)
    end markFree

    /** Include references captured by the called method in the current environment stack */
    def includeCallCaptures(sym: Symbol, resType: Type, pos: SrcPos)(using Context): Unit = resType match
      case _: MethodOrPoly => // wait until method is fully applied
      case _ =>
        if sym.exists && curEnv.isOpen then markFree(capturedVars(sym), pos)

    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      val sym = tree.symbol
      if sym.is(Method) then
        includeCallCaptures(sym, sym.info, tree.srcPos)
      else if !sym.isStatic then
        //debugShowEnvs()
        def addSelects(ref: TermRef, pt: Type): TermRef = pt match
          case pt: PathSelectionProto if ref.isTracked =>
            // if `ref` is not tracked then the selection could not give anything new
            // class SerializationProxy in stdlib-cc/../LazyListIterable.scala has an example where this matters.
            addSelects(ref.select(pt.sym).asInstanceOf[TermRef], pt.pt)
          case _ => ref
        val ref = sym.termRef
        val pathRef = addSelects(ref, pt)
        //if pathRef ne ref then
        //  println(i"add selects $ref --> $pathRef")
        markFree(sym, if false then ref else pathRef, tree.srcPos)
      super.recheckIdent(tree, pt)

    override def selectionProto(tree: Select, pt: Type)(using Context): Type =
      val sym = tree.symbol
      if !sym.isOneOf(UnstableValueFlags) && !sym.isStatic then PathSelectionProto(sym, pt)
      else super.selectionProto(tree, pt)

    /** A specialized implementation of the selection rule.
     *
     *  E |- f: T{ m: R^Cr }^{f}
     *  ------------------------
     *  E |- f.m: R^C
     *
     *  The implementation picks as `C` one of `{f}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f}` if this might subcapture Cr
     *  and Cr otherwise.
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

      val selType = recheckSelection(tree, qualType, name, disambiguate)
      val selWiden = selType.widen

      if pt == LhsProto
          || qualType.isBoxedCapturing
          || selType.isTrackableRef
          || selWiden.isBoxedCapturing
          || selWiden.captureSet.isAlwaysEmpty
      then
        selType
      else
        val qualCs = qualType.captureSet
        val selCs = selType.captureSet
        capt.println(i"pick one of $qualType, ${selType.widen}, $qualCs, $selCs ${selWiden.captureSet} in $tree")

        if qualCs.mightSubcapture(selCs)
            && !selCs.mightSubcapture(qualCs)
            && !pt.stripCapturing.isInstanceOf[SingletonType]
        then
          selWiden.stripCapturing.capturing(qualCs)
            .showing(i"alternate type for select $tree: $selType --> $result, $qualCs / $selCs", capt)
        else
          selType
    }//.showing(i"recheck sel $tree, $qualType = $result")

    /** Copy all @use annotations on method parameter symbols to the corresponding paramInfo types.
     */
    override def prepareFunction(funtpe: MethodType, meth: Symbol)(using Context): MethodType =
      val paramInfosWithUses = funtpe.paramInfos.zipWithConserve(funtpe.paramNames): (formal, pname) =>
        val paramOpt = meth.rawParamss.nestedFind(_.name == pname)
        paramOpt.flatMap(_.getAnnotation(defn.UseAnnot)) match
          case Some(ann) => AnnotatedType(formal, ann)
          case _ => formal
      funtpe.derivedLambdaType(paramInfos = paramInfosWithUses)

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val meth = tree.fun.symbol

      // Unsafe box/unbox handling, only for versions < 3.3
      def mapArgUsing(f: Type => Type) =
        val arg :: Nil = tree.args: @unchecked
        val argType0 = f(recheckStart(arg, pt))
        val argType = super.recheckFinish(argType0, arg, pt)
        super.recheckFinish(argType, tree, pt)

      if meth == defn.Caps_unsafeAssumePure then
        val arg :: Nil = tree.args: @unchecked
        val argType0 = recheck(arg, pt.capturing(CaptureSet.universal))
        val argType =
          if argType0.captureSet.isAlwaysEmpty then argType0
          else argType0.widen.stripCapturing
        capt.println(i"rechecking $arg with $pt: $argType")
        super.recheckFinish(argType, tree, pt)
      else if meth == defn.Caps_unsafeBox then
        mapArgUsing(_.forceBoxStatus(true))
      else if meth == defn.Caps_unsafeUnbox then
        mapArgUsing(_.forceBoxStatus(false))
      else if meth == defn.Caps_unsafeBoxFunArg then
        def forceBox(tp: Type): Type = tp.strippedDealias match
          case defn.FunctionOf(paramtpe :: Nil, restpe, isContextual) =>
            defn.FunctionOf(paramtpe.forceBoxStatus(true) :: Nil, restpe, isContextual)
          case tp @ RefinedType(parent, rname, rinfo: MethodType) =>
            tp.derivedRefinedType(parent, rname,
              rinfo.derivedLambdaType(
                paramInfos = rinfo.paramInfos.map(_.forceBoxStatus(true))))
          case tp @ CapturingType(parent, refs) =>
            tp.derivedCapturingType(forceBox(parent), refs)
        mapArgUsing(forceBox)
      else
        val res = super.recheckApply(tree, pt)
        includeCallCaptures(meth, res, tree.srcPos)
        res
    end recheckApply

    protected override
    def recheckArg(arg: Tree, formal: Type)(using Context): Type =
      val argType = recheck(arg, formal)
      formal match
        case AnnotatedType(formal1, ann) if ann.symbol == defn.UseAnnot =>
          capt.println(i"charging deep capture set of $arg: ${argType} = ${argType.deepCaptureSet}")
          markFree(argType.deepCaptureSet, arg.srcPos)
        case _ =>
      argType

    /** A specialized implementation of the apply rule.
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
     *  would be `Cq \union dcs(Ca)` instead.
     *  If the approximation is known to subcapture the declared result Cr, we pick it for C
     *  otherwise we pick Cr.
     */
    protected override
    def recheckApplication(tree: Apply, qualType: Type, funType: MethodType, argTypes: List[Type])(using Context): Type =
      val appType = Existential.toCap(super.recheckApplication(tree, qualType, funType, argTypes))
      val qualCaptures = qualType.captureSet
      val argCaptures =
        for (argType, formal) <- argTypes.lazyZip(funType.paramInfos) yield
          formal match
            case AnnotatedType(_, ann) if ann.symbol == defn.UseAnnot => argType.deepCaptureSet
            case _ => argType.captureSet
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
     *  This means:
     *   - Instantiate result type with actual arguments
     *   - If call is to a constructor:
     *      - remember types of arguments corresponding to tracked
     *        parameters in refinements.
     *      - add capture set of instantiated class to capture set of result type.
     *  If all argument types are mutually different trackable capture references, use a BiTypeMap,
     *  since that is more precise. Otherwise use a normal idempotent map, which might lose information
     *  in the case where the result type contains captureset variables that are further
     *  constrained afterwards.
     */
    override def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val ownType =
        if !mt.isResultDependent then
          mt.resType
        else if argTypes.forall(_.isTrackableRef) && isDistinct(argTypes) then
          SubstParamsBiMap(mt, argTypes)(mt.resType)
        else
          SubstParamsMap(mt, argTypes)(mt.resType)

      if sym.isConstructor then
        val cls = sym.owner.asClass

        /** First half of result pair:
         *  Refine the type of a constructor call `new C(t_1, ..., t_n)`
         *  to C{val x_1: T_1, ..., x_m: T_m} where x_1, ..., x_m are the tracked
         *  parameters of C and T_1, ..., T_m are the types of the corresponding arguments.
         *
         *  Second half: union of all capture sets of arguments to tracked parameters.
         */
        def addParamArgRefinements(core: Type, initCs: CaptureSet): (Type, CaptureSet) =
          var refined: Type = core
          var allCaptures: CaptureSet =
            if core.derivesFromCapability then defn.universalCSImpliedByCapability else initCs
          for (getterName, argType) <- mt.paramNames.lazyZip(argTypes) do
            val getter = cls.info.member(getterName).suchThat(_.isRefiningParamAccessor).symbol
            if !getter.is(Private) && getter.hasTrackedParts then
              refined = RefinedType(refined, getterName, argType.unboxed)
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

        augmentConstructorType(ownType, capturedVars(cls) ++ capturedVars(sym))
          .showing(i"constr type $mt with $argTypes%, % in $cls = $result", capt)
      else ownType
    end instantiate

    override def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      val meth = tree.fun match
        case fun @ Select(qual, nme.apply) => qual.symbol.orElse(fun.symbol)
        case fun => fun.symbol
      disallowCapInTypeArgs(tree.fun, meth, tree.args, thisPhase)
      val res = Existential.toCap(super.recheckTypeApply(tree, pt))
      includeCallCaptures(tree.symbol, res, tree.srcPos)
      checkContains(tree)
      res
    end recheckTypeApply

    /** Faced with a tree of form `caps.contansImpl[CS, r.type]`, check that `R` is a tracked
     *  capability and assert that `{r} <:CS`.
     */
    def checkContains(tree: TypeApply)(using Context): Unit = tree match
      case ContainsImpl(csArg, refArg) =>
        val cs = csArg.knownType.captureSet
        val ref = refArg.knownType
        capt.println(i"check contains $cs , $ref")
        ref match
          case ref: CaptureRef if ref.isTracked =>
            checkElem(ref, cs, tree.srcPos)
          case _ =>
            report.error(em"$refArg is not a tracked capability", refArg.srcPos)
      case _ =>

    override def recheckBlock(tree: Block, pt: Type)(using Context): Type =
      inNestedLevel(super.recheckBlock(tree, pt))

    override def recheckClosure(tree: Closure, pt: Type, forceDependent: Boolean)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      capt.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt, forceDependent).capturing(cs)
        .showing(i"rechecked closure $tree / $pt = $result", capt)

    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
      openClosures = (mdef.symbol, pt) :: openClosures
      try
        // Constrain closure's parameters and result from the expected type before
        // rechecking the body.
        val res = recheckClosure(expr, pt, forceDependent = true)
        if !(isEtaExpansion(mdef) && ccConfig.handleEtaExpansionsSpecially) then
          // If closure is an eta expanded method reference it's better to not constrain
          // its internals early since that would give error messages in generated code
          // which are less intelligible.
          // Example is the line `a = x` in neg-custom-args/captures/vars.scala.
          // For all other closures, early constraints are preferred since they
          // give more localized error messages.
          val res1 = Existential.toCapDeeply(res)
          val pt1 = Existential.toCapDeeply(pt)
            // We need to open existentials here in order not to get vars mixed up in them
            // We do the proper check with existentials when we are finished with the closure block.
          capt.println(i"pre-check closure $expr of type $res1 against $pt1")
          checkConformsExpr(res1, pt1, expr)
        recheckDef(mdef, mdef.symbol)
        res
      finally
        openClosures = openClosures.tail
    end recheckClosureBlock

    override def seqLiteralElemProto(tree: SeqLiteral, pt: Type, declared: Type)(using Context) =
      super.seqLiteralElemProto(tree, pt, declared).boxed

    /** Maps mutable variables to the symbols that capture them (in the
     *  CheckCaptures sense, i.e. symbol is referred to from a different method
     *  than the one it is defined in).
     */
    private val capturedBy = util.HashMap[Symbol, Symbol]()

    /** Maps anonymous functions appearing as function arguments to
     *  the function that is called.
     */
    private val anonFunCallee = util.HashMap[Symbol, Symbol]()

    /** Populates `capturedBy` and `anonFunCallee`. Called by `checkUnit`.
     */
    private def collectCapturedMutVars(using Context) = new TreeTraverser:
      def traverse(tree: Tree)(using Context) = tree match
        case id: Ident =>
          val sym = id.symbol
          if sym.is(Mutable, butNot = Method) && sym.owner.isTerm then
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

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      try
        if sym.is(Module) then sym.info // Modules are checked by checking the module class
        else
          if sym.is(Mutable) && !sym.hasAnnotation(defn.UncheckedCapturesAnnot) then
            val (carrier, addendum) = capturedBy.get(sym) match
              case Some(encl) =>
                val enclStr =
                  if encl.isAnonymousFunction then
                    val location = anonFunCallee.get(encl) match
                      case Some(meth) if meth.exists => i" argument in a call to $meth"
                      case _ => ""
                    s"an anonymous function$location"
                  else encl.show
                (NoSymbol, i"\n\nNote that $sym does not count as local since it is captured by $enclStr")
              case _ =>
                (sym, "")
            disallowRootCapabilitiesIn(
              tree.tpt.knownType, carrier, i"Mutable $sym", "have type", addendum, sym.srcPos)
          checkInferredResult(super.recheckValDef(tree, sym), tree)
      finally
        if !sym.is(Param) then
          // Parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateVarsIn(tree.tpt)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      if Synthetics.isExcluded(sym) then sym.info
      else
        // If rhs ends in a closure or anonymous class, the corresponding symbol
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
        if !localSet.isAlwaysEmpty then
          curEnv = Env(sym, EnvKind.Regular, localSet, curEnv, nestedClosure(tree.rhs))

        // ctx with AssumedContains entries for each Contains parameter
        val bodyCtx =
          var ac = CaptureSet.assumedContains
          for paramSyms <- sym.paramSymss do
            for case ContainsParam(cs, ref) <- paramSyms do
              ac = ac.updated(cs, ac.getOrElse(cs, SimpleIdentitySet.empty) + ref)
          if ac.isEmpty then ctx
          else ctx.withProperty(CaptureSet.AssumedContains, Some(ac))

        inNestedLevel: // TODO: needed here?
          try checkInferredResult(super.recheckDefDef(tree, sym)(using bodyCtx), tree)
          finally
            if !sym.isAnonymousFunction then
              // Anonymous functions propagate their type to the enclosing environment
              // so it is not in general sound to interpolate their types.
              interpolateVarsIn(tree.tpt)
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
        sym.is(Private)                   // private symbols can always have inferred types
        || sym.privateWithin == sym.effectiveOwner
        || sym.name.is(DefaultGetterName) // default getters are exempted since otherwise it would be
                                          // too annoying. This is a hole since a defualt getter's result type
                                          // might leak into a type variable.
        ||                                // non-local symbols cannot have inferred types since external capture types are not inferred
          sym.isLocalToCompilationUnit    // local symbols still need explicit types if
          && !sym.owner.is(Trait)         // they are defined in a trait, since we do OverridingPairs checking before capture inference

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
          todoAtPostCheck += (() => checkConformsExpr(tp, expected, tree.rhs, addenda(expected)))
        case _ =>
      tp
    end checkInferredResult

    /** Class-specific capture set relations:
     *   1. The capture set of a class includes the capture sets of its parents.
     *   2. The capture set of the self type of a class includes the capture set of the class.
     *   3. The capture set of the self type of a class includes the capture set of every class parameter,
     *      unless the parameter is marked @constructorOnly.
     *   4. If the class extends a pure base class, the capture set of the self type must be empty.
     */
    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(cls)
      for parent <- impl.parents do // (1)
        checkSubset(capturedVars(parent.tpe.classSymbol), localSet, parent.srcPos,
          i"\nof the references allowed to be captured by $cls")
      if !localSet.isAlwaysEmpty then
        curEnv = Env(cls, EnvKind.Regular, localSet, curEnv)
      try
        val thisSet = cls.classInfo.selfType.captureSet.withDescription(i"of the self type of $cls")
        checkSubset(localSet, thisSet, tree.srcPos) // (2)
        for param <- cls.paramGetters do
          if !param.hasAnnotation(defn.ConstructorOnlyAnnot)
            && !param.hasAnnotation(defn.UntrackedCapturesAnnot) then
            checkSubset(param.termRef.captureSet, thisSet, param.srcPos) // (3)
        for pureBase <- cls.pureBaseClass do // (4)
          def selfType = impl.body
            .collect:
              case TypeDef(tpnme.SELF, rhs) => rhs
            .headOption
            .getOrElse(tree)
            .orElse(tree)
          checkSubset(thisSet,
            CaptureSet.empty.withDescription(i"of pure base class $pureBase"),
            selfType.srcPos, cs1description = " captured by this self type")
        for case tpt: TypeTree <- impl.parents do
          tpt.tpe match
            case AppliedType(fn, args) =>
              disallowCapInTypeArgs(tpt, fn.typeSymbol, args.map(TypeTree(_)), thisPhase)
            case _ =>
        inNestedLevelUnless(cls.is(Module)):
          super.recheckClassDef(tree, impl, cls)
      finally
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
              markFree(cap.symbol, tree.srcPos)
            case _ =>
        case _ =>
      super.recheckTyped(tree)

    override def recheckTry(tree: Try, pt: Type)(using Context): Type =
      val tp = super.recheckTry(tree, pt)
      if ccConfig.useSealed && Feature.enabled(Feature.saferExceptions) then
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

    /** If expected type `pt` is boxed and the tree is a function or a reference,
     *  don't propagate free variables.
     *  Otherwise, if the result type is boxed, simulate an unboxing by
     *  adding all references in the boxed capture set to the current environment.
     */
    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val saved = curEnv
      tree match
        case _: RefTree | closureDef(_) if pt.isBoxedCapturing =>
          curEnv = Env(curEnv.owner, EnvKind.Boxed, CaptureSet.Var(curEnv.owner, level = currentLevel), curEnv)
        case _ =>
      val res =
        try
          if capt eq noPrinter then
            super.recheck(tree, pt)
          else
            trace.force(i"rechecking $tree with pt = $pt", recheckr, show = true):
              super.recheck(tree, pt)
        catch case ex: NoCommonRoot =>
          report.error(ex.getMessage.nn)
          tree.tpe
        finally curEnv = saved
      if tree.isTerm && !pt.isBoxedCapturing && pt != LhsProto then
        markFree(res.boxedCaptureSet, tree.srcPos)
      res

    override def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type =
      checkNotUniversalInUnboxedResult(tpe, tree)
      super.recheckFinish(tpe, tree, pt)
    end recheckFinish

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

    private def boxErrorAddenda(boxErrors: BoxErrors) =
      if boxErrors == null then NothingToAdd
      else new Addenda:
        override def toAdd(using Context): List[String] =
          boxErrors.toList.map: msg =>
            i"""
              |
              |Note that ${msg.toString}"""

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
      var expected1 = alignDependentFunction(expected, actual.stripCapturing)
      val boxErrors = new mutable.ListBuffer[Message]
      val actualBoxed = adapt(actual, expected1, tree.srcPos, boxErrors)
      //println(i"check conforms $actualBoxed <<< $expected1")

      if actualBoxed eq actual then
        // Only `addOuterRefs` when there is no box adaptation
        expected1 = addOuterRefs(expected1, actual, tree.srcPos)
      if isCompatible(actualBoxed, expected1) then
        if debugSuccesses then tree match
            case Ident(_) =>
              println(i"SUCCESS $tree:\n${TypeComparer.explained(_.isSubType(actual, expected))}")
            case _ =>
        actualBoxed
      else
        capt.println(i"conforms failed for ${tree}: $actual vs $expected")
        err.typeMismatch(tree.withType(actualBoxed), expected1,
            addApproxAddenda(
              addenda ++ CaptureSet.levelErrors ++ boxErrorAddenda(boxErrors),
              expected1))
        actual
    end checkConformsExpr

    /** Turn `expected` into a dependent function when `actual` is dependent. */
    private def alignDependentFunction(expected: Type, actual: Type)(using Context): Type =
      def recur(expected: Type): Type = expected.dealias match
        case expected0 @ CapturingType(eparent, refs) =>
          val eparent1 = recur(eparent)
          if eparent1 eq eparent then expected
          else CapturingType(eparent1, refs, boxed = expected0.isBoxed)
        case expected @ defn.FunctionOf(args, resultType, isContextual)
        if defn.isNonRefinedFunction(expected) =>
          actual match
            case defn.RefinedFunctionOf(rinfo: MethodType) =>
              depFun(args, resultType, isContextual, rinfo.paramNames)
            case _ => expected
        case _ => expected
      recur(expected)

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

      // Augment expeced capture set `erefs` by all references in actual capture
      // set `arefs` that are outside some `C.this.type` reference in `erefs` for an enclosing
      // class `C`. If an added reference is not a ThisType itself, add it to the capture set
      // (i.e. use set) of the `C`. This makes sure that any outer reference implicitly subsumed
      // by `C.this` becomes a capture reference of every instance of `C`.
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
    def adaptBoxed(actual: Type, expected: Type, pos: SrcPos, covariant: Boolean, alwaysConst: Boolean, boxErrors: BoxErrors)(using Context): Type =

      def recur(actual: Type, expected: Type, covariant: Boolean): Type =

        /** Adapt the inner shape type: get the adapted shape type, and the capture set leaked during adaptation
         *  @param boxed   if true we adapt to a boxed expected type
         */
        def adaptShape(actualShape: Type, boxed: Boolean): (Type, CaptureSet) = actualShape match
          case FunctionOrMethod(aargs, ares) =>
            val saved = curEnv
            curEnv = Env(
              curEnv.owner, EnvKind.NestedInOwner,
              CaptureSet.Var(curEnv.owner, level = currentLevel),
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

        actual match
          case actual @ Existential(_, actualUnpacked) =>
            return Existential.derivedExistentialType(actual):
                recur(actualUnpacked, expected, covariant)
          case _ =>
        expected match
          case expected @ Existential(_, expectedUnpacked) =>
            return recur(actual, expectedUnpacked, covariant)
          case _: WildcardType =>
            return actual
          case _ =>

        trace(adaptStr, capt, show = true) {

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
            if !leaked.subCaptures(cs, frozen = false).isOK then
              report.error(
                em"""$expected cannot be box-converted to $actual
                    |since the additional capture set $leaked resulted from box conversion is not allowed in $actual""", pos)
            cs

        // Compute the adapted type
        def adaptedType(resultBoxed: Boolean) =
          if (adaptedShape eq actualShape) && leaked.isAlwaysEmpty && actualIsBoxed == resultBoxed
          then actual
          else adaptedShape
            .capturing(if alwaysConst then CaptureSet(captures.elems) else captures)
            .forceBoxStatus(resultBoxed)

        if needsAdaptation then
          val criticalSet =          // the set which is not allowed to have `cap`
            if covariant then captures    // can't box with `cap`
            else expected.captureSet // can't unbox with `cap`
          def msg = em"""$actual cannot be box-converted to $expected
                        |since at least one of their capture sets contains the root capability `cap`"""
          def allowUniversalInBoxed =
            ccConfig.useSealed
            || expected.hasAnnotation(defn.UncheckedCapturesAnnot)
            || actual.widen.hasAnnotation(defn.UncheckedCapturesAnnot)
          if criticalSet.isUnboxable && expected.isValueType && !allowUniversalInBoxed then
            // We can't box/unbox the universal capability. Leave `actual` as it is
            // so we get an error in checkConforms. Add the error message generated
            // from boxing as an addendum. This tends to give better error
            // messages than disallowing the root capability in `criticalSet`.
            if boxErrors != null then boxErrors += msg
            if ctx.settings.YccDebug.value then
              println(i"cannot box/unbox $actual vs $expected")
            actual
          else
            if !allowUniversalInBoxed then
              // Disallow future addition of `cap` to `criticalSet`.
              criticalSet.disallowRootCapability: () =>
                report.error(msg, pos)
            if !insertBox then  // unboxing
              //debugShowEnvs()
              markFree(criticalSet, pos)
            adaptedType(!actualIsBoxed)
        else
          adaptedType(actualIsBoxed)
        }
      end recur

      recur(actual, expected, covariant)
    end adaptBoxed

    /** If actual is a tracked CaptureRef `a` and widened is a capturing type T^C,
     *  improve `T^C` to `T^{a}`, following the VAR rule of CC.
     *  TODO: We probably should do this also for other top-level occurrences of captures
     *  E.g.
     *    class Foo { def a: C^{io}; val def: C^{async} }
     *    val foo: Foo^{io, async}
     *  Then
     *    foo: Foo { def a: C^{foo}; def b: C^{foo} }^{foo}
     */
    private def improveCaptures(widened: Type, actual: Type)(using Context): Type = actual match
      case ref: CaptureRef if ref.isTracked =>
        widened match
          case CapturingType(p, refs) if ref.singletonCaptureSet.mightSubcapture(refs) =>
            widened.derivedCapturingType(p, ref.singletonCaptureSet)
              .showing(i"improve $widened to $result", capt)
          case _ => widened
      case _ => widened

    /** Adapt `actual` type to `expected` type by inserting boxing and unboxing conversions
     *
     *  @param alwaysConst  always make capture set variables constant after adaptation
     */
    def adapt(actual: Type, expected: Type, pos: SrcPos, boxErrors: BoxErrors)(using Context): Type =
      if expected == LhsProto || expected.isSingleton && actual.isSingleton then
        actual
      else
        val widened = improveCaptures(actual.widen.dealiasKeepAnnots, actual)
        val adapted = adaptBoxed(
            widened.withReachCaptures(actual), expected, pos,
            covariant = true, alwaysConst = false, boxErrors)
        if adapted eq widened then actual
        else adapted.showing(i"adapt boxed $actual vs $expected = $adapted", capt)
    end adapt

    /** Check overrides again, taking capture sets into account.
    *  TODO: Can we avoid doing overrides checks twice?
    *  We need to do them here since only at this phase CaptureTypes are relevant
    *  But maybe we can then elide the check during the RefChecks phase under captureChecking?
    */
    def checkOverrides = new TreeTraverser:
      class OverridingPairsCheckerCC(clazz: ClassSymbol, self: Type, srcPos: SrcPos)(using Context) extends OverridingPairsChecker(clazz, self):
        /** Check subtype with box adaptation.
        *  This function is passed to RefChecks to check the compatibility of overriding pairs.
        *  @param sym  symbol of the field definition that is being checked
        */
        override def checkSubType(actual: Type, expected: Type)(using Context): Boolean =
          val expected1 = alignDependentFunction(addOuterRefs(expected, actual, srcPos), actual.stripCapturing)
          val actual1 =
            val saved = curEnv
            try
              curEnv = Env(clazz, EnvKind.NestedInOwner, capturedVars(clazz), outer0 = curEnv)
              val adapted =
                adaptBoxed(actual, expected1, srcPos, covariant = true, alwaysConst = true, null)
              actual match
                case _: MethodType =>
                  // We remove the capture set resulted from box adaptation for method types,
                  // since class methods are always treated as pure, and their captured variables
                  // are charged to the capture set of the class (which is already done during
                  // box adaptation).
                  adapted.stripCapturing
                case _ => adapted
            finally curEnv = saved
          actual1 frozen_<:< expected1

        override def needsCheck(overriding: Symbol, overridden: Symbol)(using Context): Boolean =
          !setup.isPreCC(overriding) && !setup.isPreCC(overridden)

        override def checkInheritedTraitParameters: Boolean = false

        /** Check that overrides don't change the @use status of their parameters */
        override def additionalChecks(member: Symbol, other: Symbol)(using Context): Unit =
          for
            (params1, params2) <- member.rawParamss.lazyZip(other.rawParamss)
            (param1, param2) <- params1.lazyZip(params2)
          do
            if param1.hasAnnotation(defn.UseAnnot) != param2.hasAnnotation(defn.UseAnnot) then
              report.error(
                OverrideError(
                    i"has a parameter ${param1.name} with different @use status than the corresponding parameter in the overridden definition",
                    self, member, other, self.memberInfo(member), self.memberInfo(other)
                  ),
                if member.owner == clazz then member.srcPos else clazz.srcPos
              )
      end OverridingPairsCheckerCC

      def traverse(t: Tree)(using Context) =
        t match
          case t: Template =>
            checkAllOverrides(ctx.owner.asClass, OverridingPairsCheckerCC(_, _, t))
          case _ =>
        traverseChildren(t)

    private val completed = new mutable.HashSet[Symbol]

    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    /** Check a ValDef or DefDef as an action performed in a completer. Since
     *  these checks can appear out of order, we need to firsty create the correct
     *  environment for checking the definition.
     */
    def completeDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      val saved = curEnv
      try
        // Setup environment to reflect the new owner.
        val envForOwner: Map[Symbol, Env] = curEnv.outersIterator
          .takeWhile(e => !capturedVars(e.owner).isAlwaysEmpty) // no refs can leak beyind this point
          .map(e => (e.owner, e))
          .toMap
        def restoreEnvFor(sym: Symbol): Env =
          val localSet = capturedVars(sym)
          if localSet.isAlwaysEmpty then rootEnv
          else envForOwner.get(sym) match
            case Some(e) => e
            case None => Env(sym, EnvKind.Regular, localSet, restoreEnvFor(sym.owner))
        curEnv = restoreEnvFor(sym.owner)
        capt.println(i"Complete $sym in ${curEnv.outersIterator.toList.map(_.owner)}")
        try recheckDef(tree, sym)
        finally completed += sym
      finally
        curEnv = saved

    private val setup: SetupAPI = thisPhase.prev.asInstanceOf[Setup]

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      setup.setupUnit(unit.tpdTree, completeDef)
      collectCapturedMutVars.traverse(unit.tpdTree)

      if ctx.settings.YccPrintSetup.value then
        val echoHeader = "[[syntax tree at end of cc setup]]"
        val treeString = show(unit.tpdTree)
        report.echo(s"$echoHeader\n$treeString\n")

      withCaptureSetsExplained:
        super.checkUnit(unit)
        checkOverrides.traverse(unit.tpdTree)
        postCheck(unit.tpdTree)
        checkSelfTypes(unit.tpdTree)
        postCheckWF(unit.tpdTree)
        if ctx.settings.YccDebug.value then
          show(unit.tpdTree) // this does not print tree, but makes its variables visible for dependency printing

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
            interpolator(startingVariance = -1).traverse(selfType)
            selfType match
              case CapturingType(_, refs: CaptureSet.Var)
              if !root.isEffectivelySealed
                  && !refs.elems.exists(_.isRootCapability)
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

    /** Heal ill-formed capture sets in the type parameter.
     *
     *  We can push parameter refs into a capture set in type parameters
     *  that this type parameter can't see.
     *  For example, when capture checking the following expression:
     *
     *    def usingLogFile[T](op: File^ => T): T = ...
     *
     *    usingLogFile[box ?1 () -> Unit] { (f: File^) => () => { f.write(0) } }
     *
     *  We may propagate `f` into ?1, making ?1 ill-formed.
     *  This also causes soundness issues, since `f` in ?1 should be widened to `cap`,
     *  giving rise to an error that `cap` cannot be included in a boxed capture set.
     *
     *  To solve this, we still allow ?1 to capture parameter refs like `f`, but
     *  compensate this by pushing the widened capture set of `f` into ?1.
     *  This solves the soundness issue caused by the ill-formness of ?1.
     */
    private def healTypeParam(tree: Tree, paramName: TypeName, meth: Symbol)(using Context): Unit =
      val checker = new TypeTraverser:
        private var allowed: SimpleIdentitySet[TermParamRef] = SimpleIdentitySet.empty

        private def isAllowed(ref: CaptureRef): Boolean = ref match
          case ref: TermParamRef => allowed.contains(ref)
          case _ => true

        private def healCaptureSet(cs: CaptureSet): Unit =
          cs.ensureWellformed: elem =>
            ctx ?=>
              var seen = new util.HashSet[CaptureRef]
              def recur(ref: CaptureRef): Unit = ref.stripReach match
                case ref: TermParamRef
                if !allowed.contains(ref) && !seen.contains(ref) =>
                  seen += ref
                  if ref.isMaxCapability then
                    report.error(i"escaping local reference $ref", tree.srcPos)
                  else
                    val widened = ref.captureSetOfInfo
                    val added = widened.filter(isAllowed(_))
                    capt.println(i"heal $ref in $cs by widening to $added")
                    if !added.subCaptures(cs, frozen = false).isOK then
                      val location = if meth.exists then i" of ${meth.showLocated}" else ""
                      val paramInfo =
                        if ref.paramName.info.kind.isInstanceOf[UniqueNameKind]
                        then i"${ref.paramName} from ${ref.binder}"
                        else i"${ref.paramName}"
                      val debugSetInfo = if ctx.settings.YccDebug.value then i" $cs" else ""
                      report.error(
                        i"local reference $paramInfo leaks into outer capture set$debugSetInfo of type parameter $paramName$location",
                        tree.srcPos)
                    else
                      widened.elems.foreach(recur)
                case _ =>
              recur(elem)

        def traverse(tp: Type) =
          tp match
            case CapturingType(parent, refs) =>
              healCaptureSet(refs)
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
        checker.traverse(tree.knownType)
    end healTypeParam

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

    /** Perform the following kinds of checks
     *   - Check that arguments of TypeApplys and AppliedTypes conform to their bounds.
     *   - Heal ill-formed capture sets of type parameters. See `healTypeParam`.
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
            fun.knownType.widen match
              case tl: PolyType =>
                val normArgs = args.lazyZip(tl.paramInfos).map: (arg, bounds) =>
                  arg.withType(arg.knownType.forceBoxStatus(
                    bounds.hi.isBoxedCapturing | bounds.lo.isBoxedCapturing))
                checkBounds(normArgs, tl)
                args.lazyZip(tl.paramNames).foreach(healTypeParam(_, _, fun.symbol))
              case _ =>
          case tree: TypeTree =>
            checkArraysAreSealedIn(tree.tpe, tree.srcPos)
          case _ =>
        end check
      end checker

      checker.traverse(unit)(using ctx.withOwner(defn.RootClass))
      if !ctx.reporter.errorsReported then
        // We dont report errors here if previous errors were reported, because other
        // errors often result in bad applied types, but flagging these bad types gives
        // often worse error messages than the original errors.
        val checkApplied = new TreeTraverser:
          def traverse(t: Tree)(using Context) = t match
            case tree: InferredTypeTree =>
            case tree: New =>
            case tree: TypeTree => checkAppliedTypesIn(tree.withKnownType)
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
