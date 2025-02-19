package dotty.tools
package dotc
package typer

import core.*
import ast.*
import Contexts.*, Types.*, Denotations.*, Names.*, StdNames.*, NameOps.*, Symbols.*
import NameKinds.DepParamName
import Trees.*
import Constants.*
import util.{Stats, SimpleIdentityMap, SimpleIdentitySet}
import Decorators.*
import Uniques.*
import Flags.{Method, Transparent}
import inlines.Inlines
import config.{Feature, SourceVersion}
import config.Printers.typr
import Inferencing.*
import ErrorReporting.*
import util.SourceFile
import util.Spans.{NoSpan, Span}
import TypeComparer.necessarySubType
import reporting.*

import scala.annotation.internal.sharable

object ProtoTypes {

  import tpd.*

  /** A trait defining an `isCompatible` method. */
  trait Compatibility:

    /** Is there an implicit conversion from `tp` to `pt`? */
    def viewExists(tp: Type, pt: Type)(using Context): Boolean

    /** A type `tp` is compatible with a type `pt` if one of the following holds:
     *    1. `tp` is a subtype of `pt`
     *    2. `pt` is by name parameter type, and `tp` is compatible with its underlying type
     *    3. there is an implicit conversion from `tp` to `pt`.
     *    4. `tp` is a numeric subtype of `pt` (this case applies even if implicit conversions are disabled)
     *  If `pt` is a by-name type, we compare against the underlying type instead.
     */
    def isCompatible(tp: Type, pt: Type)(using Context): Boolean =
      (tp.widenExpr relaxed_<:< pt.widenExpr) || viewExists(tp, pt)

    /** Like normalize and then isCompatible, but using a subtype comparison with
     *  necessary eithers that does not unnecessarily truncate the constraint space,
     *  returning false instead.
     */
    def necessarilyCompatible(tp: Type, pt: Type)(using Context): Boolean =
      val tpn = normalize(tp, pt, followIFT = !defn.isContextFunctionType(pt))
      necessarySubType(tpn, pt) || tpn.isValueSubType(pt) || viewExists(tpn, pt)

    /** Test compatibility after normalization.
     *  If `keepConstraint` is false, the current constraint set will not be modified by this call.
     */
    def normalizedCompatible(tp: Type, pt: Type, keepConstraint: Boolean)(using Context): Boolean =

      def testCompat(using Context): Boolean =
        val normTp = normalize(tp, pt)
        isCompatible(normTp, pt) || pt.isRef(defn.UnitClass) && normTp.isParameterless

      if keepConstraint then
        tp.widenSingleton match
          case poly: PolyType =>
            val newctx = ctx.fresh.setNewTyperState()
            val result = testCompat(using newctx)
            typr.println(
                i"""normalizedCompatible for $poly, $pt = $result
                   |constraint was: ${ctx.typerState.constraint}
                   |constraint now: ${newctx.typerState.constraint}""")
            if result && (ctx.typerState.constraint ne newctx.typerState.constraint) then
              // Remove all type lambdas and tvars introduced by testCompat
              for tvar <- newctx.typerState.ownedVars do
                inContext(newctx):
                  if !tvar.isInstantiated then
                    // Filter out any tvar that instantiating would further constrain the current constraint
                    // Similar to filterByDeps in interpolateTypeVars.
                    val excluded = ctx.typerState.ownedVars.filter(!_.isInstantiated)
                    val aboveOK = !ctx.typerState.constraint.dependsOn(tvar, excluded, co = true)
                    val belowOK = !ctx.typerState.constraint.dependsOn(tvar, excluded, co = false)
                    if belowOK then
                      tvar.instantiate(fromBelow = true)
                    else if aboveOK then
                      tvar.instantiate(fromBelow = false)

              // commit any remaining changes in typer state
              newctx.typerState.commit()
            result
          case _ => testCompat
      else explore(testCompat)
    end normalizedCompatible

    private def disregardProto(pt: Type)(using Context): Boolean =
      pt.dealias.isRef(defn.UnitClass)

    /** Check that the result type of the current method
     *  fits the given expected result type.
     */
    def constrainResult(mt: Type, pt: Type)(using Context): Boolean =
    trace(i"constrainResult($mt, $pt)", typr):
      val savedConstraint = ctx.typerState.constraint
      val res = pt.widenExpr match {
        case pt: FunProto =>
          mt match
            case mt: MethodType =>
              constrainResult(resultTypeApprox(mt), pt.resultType)
              && {
                if pt.constrainResultDeep
                   && mt.isImplicitMethod == (pt.applyKind == ApplyKind.Using)
                then
                  pt.args.lazyZip(mt.paramInfos).forall((arg, paramInfo) =>
                    pt.typedArg(arg, paramInfo).tpe <:< paramInfo)
                else true
              }
            case _ => true
        case _: ValueTypeOrProto if !disregardProto(pt) =>
          necessarilyCompatible(mt, pt)
        case pt: WildcardType if pt.optBounds.exists =>
          necessarilyCompatible(mt, pt)
        case _ =>
          true
      }
      if !res then ctx.typerState.constraint = savedConstraint
      res

    /** Constrain result with two special cases:
     *   1. If `meth` is a transparent inlineable method in an inlineable context,
     *      we should always succeed and not constrain type parameters in the expected type,
     *      because the actual return type can be a subtype of the currently known return type.
     *      However, we should constrain parameters of the declared return type. This distinction is
     *      achieved by replacing expected type parameters with wildcards.
     *   2. When constraining the result of a primitive value operation against
     *      a precise typevar, don't lower-bound the typevar with a non-singleton type.
     */
    def constrainResult(meth: Symbol, mt: Type, pt: Type)(using Context): Boolean =

      def constFoldException(pt: Type): Boolean = pt.dealias match
        case tvar: TypeVar =>
          tvar.isPrecise
          && meth.is(Method) && meth.owner.isPrimitiveValueClass
          && mt.resultType.isPrimitiveValueType && !mt.resultType.isSingleton
        case tparam: TypeParamRef =>
          constFoldException(ctx.typerState.constraint.typeVarOfParam(tparam))
        case _ =>
          false

      constFoldException(pt) || {
        if Inlines.isInlineable(meth) then
          // Stricter behavisour in 3.4+: do not apply `wildApprox` to non-transparent inlines
          // unless their return type is a MatchType. In this case there's no reason
          // not to constrain type variables in the expected type. For transparent inlines
          // we do not want to constrain type variables in the expected type since the
          // actual return type might be smaller after instantiation. For inlines returning
          // MatchTypes we do not want to constrain because the MatchType might be more
          // specific after instantiation. TODO: Should we also use Wildcards for non-inline
          // methods returning MatchTypes?
          if Feature.sourceVersion.isAtLeast(SourceVersion.`3.4`) then
            if meth.is(Transparent) || mt.resultType.isMatchAlias then
              constrainResult(mt, wildApprox(pt))
              // do not constrain the result type of transparent inline methods
              true
            else
              constrainResult(mt, pt)
          else
            // Best-effort to fix https://github.com/scala/scala3/issues/9685 in the 3.3.x series
            // while preserving source compatibility as much as possible
            constrainResult(mt, wildApprox(pt)) || meth.is(Transparent)
        else constrainResult(mt, pt)
      }

    end constrainResult
  end Compatibility

  object NoViewsAllowed extends Compatibility {
    override def viewExists(tp: Type, pt: Type)(using Context): Boolean = false
  }

  /** A trait for prototypes that match all types */
  trait MatchAlways extends ProtoType {
    def isMatchedBy(tp1: Type, keepConstraint: Boolean)(using Context): Boolean = true
    def map(tm: TypeMap)(using Context): ProtoType = this
    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T = x
    override def toString: String = getClass.toString
  }

  /** A class marking ignored prototypes that can be revealed by `deepenProto` */
  abstract case class IgnoredProto(ignored: Type) extends CachedGroundType with MatchAlways:
    private var myWasDeepened = false
    override def revealIgnored = ignored
    override def deepenProto(using Context): Type =
      myWasDeepened = true
      ignored
    override def deepenProtoTrans(using Context): Type = ignored.deepenProtoTrans

    /** Did someone look inside via deepenProto? Used for error deagniostics
     *  to give a more extensive expected type.
     */
    def wasDeepened: Boolean = myWasDeepened

    override def computeHash(bs: Hashable.Binders): Int = doHash(bs, ignored)

    override def eql(that: Type): Boolean = that match
      case that: IgnoredProto => ignored eq that.ignored
      case _ => false

    // equals comes from case class; no need to redefine
  end IgnoredProto

  final class CachedIgnoredProto(ignored: Type) extends IgnoredProto(ignored)

  object IgnoredProto:
    def apply(ignored: Type)(using Context): IgnoredProto = ignored match
      case ignored: IgnoredProto => ignored
      case _ => unique(CachedIgnoredProto(ignored))

  /** A prototype for expressions [] that are part of a selection operation:
   *
   *       [ ].name: proto
   */
  abstract case class SelectionProto(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean, nameSpan: Span)
  extends CachedProxyType with ProtoType with ValueTypeOrProto {

    /** Is the set of members of this type unknown, in the sense that we
     *  cannot compute a non-trivial upper approximation? This is the case if:
     *    1. The type has Nothing or Wildcard as a prefix or underlying type
     *    2. The type is an abstract type with a lower bound that has a unknown
     *       members and an upper bound that is both provisional and has unknown members.
     *    3. The type is an uninstiated type var with a lower that has unknown members.
     *    4. Type proxies have unknown members if their super types do
     */
    private def hasUnknownMembers(tp: Type)(using Context): Boolean = tp match
      case tp: WildcardType => true
      case NoType => true
      case tp: TypeRef =>
        val sym = tp.symbol
        sym == defn.NothingClass
        ||     !sym.isClass
            && !sym.isStatic
            && {
                hasUnknownMembers(tp.prefix)
                || { val bound = tp.info.hiBound
                     bound.isProvisional && hasUnknownMembers(bound)
                  } && hasUnknownMembers(tp.info.loBound)
              }
      case tp: TypeVar if !tp.isInstantiated =>
        hasUnknownMembers(TypeComparer.bounds(tp.origin).lo)
      case tp: AppliedType => hasUnknownMembers(tp.tycon) || hasUnknownMembers(tp.superType)
      case tp: TypeProxy => hasUnknownMembers(tp.superType)
      // It woukd make sense to also include And/OrTypes, but that leads to
      // infinite recursions, as observed for instance for t2399.scala.
      case _ => false

    override def isMatchedBy(tp1: Type, keepConstraint: Boolean)(using Context): Boolean =
      name == nme.WILDCARD
      || hasUnknownMembers(tp1)
      || {
        try
          val mbr = if privateOK then tp1.member(name) else tp1.nonPrivateMember(name)
          def qualifies(m: SingleDenotation) =
            val isAccessible = !m.symbol.exists || m.symbol.isAccessibleFrom(tp1, superAccess = true)
            isAccessible
            && (memberProto.isRef(defn.UnitClass)
              || tp1.isValueType && compat.normalizedCompatible(NamedType(tp1, name, m), memberProto, keepConstraint))
                // Note: can't use `m.info` here because if `m` is a method, `m.info`
                //       loses knowledge about `m`'s default arguments.
          mbr.hasAltWithInline(qualifies)
        catch case ex: TypeError =>
          // A scenario where this can happen is in pos/15673.scala:
          // We have a type `CC[A]#C` where `CC`'s upper bound is `[X] => Any`, but
          // the current constraint stipulates CC <: SeqOps[...], where `SeqOps` defines
          // the `C` parameter. We try to resolve this using `argDenot` but `argDenot`
          // consults the base type of `CC`, which is not `SeqOps`, so it does not
          // find a corresponding argument. In fact, `argDenot` is not allowed to
          // consult short-lived things like the current constraint, so it has no other
          // choice. The problem will be healed later, when normal selection fails
          // and we try to instantiate type variables to compensate. But we have to make
          // sure we do not issue a type error before we get there.
          false
      }

    def underlying(using Context): Type = WildcardType

    def derivedSelectionProto(name: Name, memberProto: Type, compat: Compatibility, nameSpan: Span)(using Context): SelectionProto =
      if ((name eq this.name) && (memberProto eq this.memberProto) && (compat eq this.compat) && (nameSpan == this.nameSpan)) this
      else SelectionProto(name, memberProto, compat, privateOK, nameSpan)

    override def isErroneous(using Context): Boolean =
      memberProto.isErroneous

    override def unusableForInference(using Context): Boolean =
      memberProto.unusableForInference

    def map(tm: TypeMap)(using Context): SelectionProto = derivedSelectionProto(name, tm(memberProto), compat, nameSpan)
    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T = ta(x, memberProto)

    override def deepenProto(using Context): SelectionProto =
      derivedSelectionProto(name, memberProto.deepenProto, compat, nameSpan)

    override def deepenProtoTrans(using Context): SelectionProto =
      derivedSelectionProto(name, memberProto.deepenProtoTrans, compat, nameSpan)

    override def computeHash(bs: Hashable.Binders): Int = {
      val delta = (if (compat eq NoViewsAllowed) 1 else 0) | (if (privateOK) 2 else 0)
      addDelta(doHash(bs, name, memberProto), delta)
    }

    override def equals(that: Any): Boolean = that match
      case that: SelectionProto =>
        (name eq that.name) && memberProto.equals(that.memberProto) && (compat eq that.compat) && (privateOK == that.privateOK)
      case _ =>
        false

    override def eql(that: Type): Boolean = that match {
      case that: SelectionProto =>
        (name eq that.name) && (memberProto eq that.memberProto) && (compat eq that.compat) && (privateOK == that.privateOK)
      case _ =>
        false
    }
  }

  class CachedSelectionProto(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean, nameSpan: Span)
  extends SelectionProto(name, memberProto, compat, privateOK, nameSpan)

  object SelectionProto {
    def apply(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean, nameSpan: Span)(using Context): SelectionProto = {
      val selproto = new CachedSelectionProto(name, memberProto, compat, privateOK, nameSpan)
      if (compat eq NoViewsAllowed) unique(selproto) else selproto
    }
  }

  /** Create a selection proto-type, but only one level deep;
   *  treat constructors specially
   */
  def shallowSelectionProto(name: Name, tp: Type, typer: Typer, nameSpan: Span)(using Context): TermType =
    if (name.isConstructorName) WildcardType
    else tp match
      case tp: UnapplyFunProto => new UnapplySelectionProto(name, nameSpan)
      case tp => SelectionProto(name, IgnoredProto(tp), typer, privateOK = true, nameSpan)

  class WildcardSelectionProto extends SelectionProto(nme.WILDCARD, WildcardType, NoViewsAllowed, true, NoSpan)

  /** A prototype for expressions [] that are in some unspecified selection operation
   *
   *    [].?: ?
   *
   *  Used to indicate that expression is in a context where the only valid
   *  operation is further selection. In this case, the expression need not be a value.
   *  @see checkValue
   */
  @sharable object AnySelectionProto extends WildcardSelectionProto

  @sharable object SingletonTypeProto extends WildcardSelectionProto

  /** A prototype for selections in pattern constructors */
  class UnapplySelectionProto(name: Name, nameSpan: Span) extends SelectionProto(name, WildcardType, NoViewsAllowed, true, nameSpan)

  trait ApplyingProto extends ProtoType   // common trait of ViewProto and FunProto
  trait FunOrPolyProto extends ProtoType: // common trait of PolyProto and FunProto
    def applyKind: ApplyKind = ApplyKind.Regular

  class FunProtoState {

    /** The list of typed arguments, if all arguments are typed */
    var typedArgs: List[Tree] = Nil

    /** A map in which typed arguments can be stored to be later integrated in `typedArgs`. */
    var typedArg: SimpleIdentityMap[untpd.Tree, Tree] = SimpleIdentityMap.empty

    /** The argument that produced errors during typing */
    var errorArgs: SimpleIdentitySet[untpd.Tree] = SimpleIdentitySet.empty

    /** The tupled or untupled version of this prototype, if it has been computed */
    var tupledDual: Type = NoType

    /** If true, the application of this prototype was canceled. */
    var toDrop: Boolean = false
  }

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType
   *
   *  @param  args      The untyped arguments to which the function is applied
   *  @param  resType   The expeected result type
   *  @param  typer     The typer to use for typing the arguments
   *  @param  applyKind The kind of application (regular/using/tupled infix operand)
   *  @param  state     The state object to use for tracking the changes to this prototype
   *  @param  constrainResultDeep
   *                    A flag to indicate that constrainResult on this prototype
   *                    should typecheck and compare the arguments.
   */
  case class FunProto(args: List[untpd.Tree], resType: Type)(
    typer: Typer,
    override val applyKind: ApplyKind,
    state: FunProtoState = new FunProtoState,
    val constrainResultDeep: Boolean = false)(using protoCtx: Context)
  extends UncachedGroundType with ApplyingProto with FunOrPolyProto {
    override def resultType(using Context): Type = resType

    def isMatchedBy(tp: Type, keepConstraint: Boolean)(using Context): Boolean = {
      val args = typedArgs()
      def isPoly(tree: Tree) = tree.tpe.widenSingleton.isInstanceOf[PolyType]
      // See remark in normalizedCompatible for why we can't keep the constraint
      // if one of the arguments has a PolyType.
      typer.isApplicableType(tp, args, resultType, keepConstraint && !args.exists(isPoly))
    }

    def derivedFunProto(
        args: List[untpd.Tree] = this.args,
        resultType: Type = this.resultType,
        typer: Typer = this.typer,
        constrainResultDeep: Boolean = this.constrainResultDeep): FunProto =
      if (args eq this.args)
          && (resultType eq this.resultType)
          && (typer eq this.typer)
          && constrainResultDeep == this.constrainResultDeep
      then this
      else new FunProto(args, resultType)(typer, applyKind, constrainResultDeep = constrainResultDeep)

    /** @return True if all arguments have types.
     */
    def allArgTypesAreCurrent()(using Context): Boolean =
      state.typedArg.size == args.length

    private def isUndefined(tp: Type): Boolean = tp.dealias match {
      case _: WildcardType => true
      case defn.FunctionNOf(args, result, _) => args.exists(isUndefined) || isUndefined(result)
      case _ => false
    }

    /** Did an argument produce an error when typing? This means: an error was reported
     *  and a tree got an error type. Errors of adaptation whree a tree has a good type
     *  but that type does not conform to the expected type are not counted.
     */
    def hasErrorArg = !state.errorArgs.isEmpty

    /** Does tree have embedded error trees that are not at the outside.
     *  A nested tree t1 is "at the outside" relative to a tree t2 if
     *    - t1 and t2 have the same span, or
     *    - t2 is a ascription (t22: T) and t1 is at the outside of t22
     *    - t2 is a closure (...) => t22 and t1 is at the outside of t22
     */
    def hasInnerErrors(t: Tree)(using Context): Boolean = t match
      case Typed(expr, tpe) => hasInnerErrors(expr)
      case closureDef(mdef) => hasInnerErrors(mdef.rhs)
      case _ =>
        t.existsSubTree { t1 =>
          if t1.typeOpt.isError
            && t.span.toSynthetic != t1.span.toSynthetic
            && t.typeOpt != t1.typeOpt then
            typr.println(i"error subtree $t1 of $t with ${t1.typeOpt}, spans = ${t1.span}, ${t.span}")
            true
          else
            false
        }

    private def cacheTypedArg(arg: untpd.Tree, typerFn: untpd.Tree => Tree, force: Boolean)(using Context): Tree = {
      var targ = state.typedArg(arg)
      if (targ == null)
        untpd.functionWithUnknownParamType(arg) match {
          case Some(untpd.Function(args, _)) if !force =>
            // If force = false, assume what we know about the parameter types rather than reporting an error.
            // That way we don't cause a "missing parameter" error in `typerFn(arg)`
            val paramTypes = args map {
              case ValDef(_, tpt, _) if !tpt.isEmpty => typer.typedType(tpt).typeOpt
              case _ => WildcardType
            }
            targ = arg.withType(defn.FunctionNOf(paramTypes, WildcardType))
          case Some(_) if !force =>
            targ = arg.withType(WildcardType)
          case _ =>
            targ = typerFn(arg)
            // TODO: investigate why flow typing is not working on `targ`
            if ctx.reporter.hasUnreportedErrors then
              if hasInnerErrors(targ.nn) then
                state.errorArgs += arg
            else
              state.typedArg = state.typedArg.updated(arg, targ.nn)
              state.errorArgs -= arg
        }
      targ.nn
    }

    /** The typed arguments. This takes any arguments already typed using
     *  `typedArg` into account.
     *
     *  Arguments are typechecked in the typerState where the FunProto was created.
     *  However, any constraint changes are also propagated to the currently passed
     *  context.
     *
     *  @param norm   a normalization function that is applied to an untyped argument tree
     *                before it is typed. The second Int parameter is the parameter index.
     */
    def typedArgs(norm: (untpd.Tree, Int) => untpd.Tree = sameTree)(using Context): List[Tree] =
      if state.typedArgs.size == args.length then state.typedArgs
      else
        val passedCtx = ctx
        val passedTyperState = ctx.typerState
        inContext(protoCtx.withUncommittedTyperState) {
          val protoTyperState = ctx.typerState
          val oldConstraint = protoTyperState.constraint
          val args1 = args.mapWithIndexConserve((arg, idx) =>
            cacheTypedArg(arg, arg => typer.typed(norm(arg, idx)), force = false))
          val newConstraint = protoTyperState.constraint

          if !args1.exists(arg => isUndefined(arg.tpe)) then state.typedArgs = args1

          // We only need to propagate constraints if we typed the arguments in a different
          // TyperState and if that created additional constraints.
          if (passedTyperState ne protoTyperState) && (oldConstraint ne newConstraint) then
            // To respect the pre-condition of `mergeConstraintWith` and keep
            // `protoTyperState` committable we must ensure that it does not
            // contain any type variable which don't already exist in the passed
            // TyperState. This is achieved by instantiating any such type
            // variable. NOTE: this does not suffice to discard type variables
            // in ancestors of `protoTyperState`, if this situation ever
            // comes up, an assertion in TyperState will trigger and this code
            // will need to be generalized.
            if protoTyperState.isCommittable then
              val passedConstraint = passedTyperState.constraint
              val newLambdas = newConstraint.domainLambdas.filter(tl =>
                !passedConstraint.contains(tl) || passedConstraint.hasConflictingTypeVarsFor(tl, newConstraint))
              val newTvars = newLambdas.flatMap(_.paramRefs).map(newConstraint.typeVarOfParam)

              args1.foreach(arg => Inferencing.instantiateSelected(arg.tpe, newTvars))

              // `instantiateSelected` can leave some type variables uninstantiated,
              // so we maximize them in a second pass.
              newTvars.foreach {
                case tvar: TypeVar if !tvar.isInstantiated =>
                  tvar.instantiate(fromBelow = false)
                case _ =>
              }
            passedTyperState.mergeConstraintWith(protoTyperState)(using passedCtx)
          end if
          args1
        }

    /** Type single argument and remember the unadapted result in `myTypedArg`.
     *  used to avoid repeated typings of trees when backtracking.
     */
    def typedArg(arg: untpd.Tree, formal: Type)(using Context): Tree = {
      val wideFormal = formal.widenExpr
      val argCtx =
        if wideFormal eq formal then ctx.retractMode(Mode.InAnnotation)
        else ctx.retractMode(Mode.InAnnotation).withNotNullInfos(ctx.notNullInfos.retractMutables)
      val locked = ctx.typerState.ownedVars
      val targ = cacheTypedArg(arg,
        typer.typedUnadapted(_, wideFormal, locked)(using argCtx),
        force = true)
      val targ1 = typer.adapt(targ, wideFormal, locked)
      if wideFormal eq formal then targ1
      else checkNoWildcardCaptureForCBN(targ1)
    }

    def checkNoWildcardCaptureForCBN(targ1: Tree)(using Context): Tree = {
      if hasCaptureConversionArg(targ1.tpe) then
        val tp = stripCast(targ1).tpe
        errorTree(targ1,
          em"""argument for by-name parameter is not a value
              |and contains wildcard arguments: $tp
              |
              |Assign it to a val and pass that instead.
              |""")
      else targ1
    }

    /** The type of the argument `arg`, or `NoType` if `arg` has not been typed before
     *  or if `arg`'s typing produced a type error.
     */
    def typeOfArg(arg: untpd.Tree)(using Context): Type = {
      val t = state.typedArg(arg)
      if (t == null) NoType else t.tpe
    }

    /** Cache the typed argument */
    def cacheArg(arg: untpd.Tree, targ: Tree) =
      state.typedArg = state.typedArg.updated(arg, targ)

    /** The same proto-type but with all arguments combined in a single tuple */
    def tupledDual: FunProto = state.tupledDual match {
      case pt: FunProto =>
        pt
      case _ =>
        val dualArgs = args match
          case untpd.Tuple(elems) :: Nil => elems
          case _ => untpd.Tuple(args) :: Nil
        state.tupledDual = new FunProto(dualArgs, resultType)(typer, applyKind)
        tupledDual
    }

    /** Somebody called the `tupledDual` method of this prototype */
    def hasTupledDual: Boolean = state.tupledDual.isInstanceOf[FunProto]

    /** Cancel the application of this prototype. This can happen for a nullary
     *  application `f()` if `f` refers to a symbol that exists both in parameterless
     *  form `def f` and nullary method form `def f()`. A common example for such
     *  a method is `toString`. If in that case the type in the denotation is
     *  parameterless, we compensate by dropping the application.
     */
    def markAsDropped(): Unit = {
      assert(args.isEmpty)
      state.toDrop = true
    }

    def isDropped: Boolean = state.toDrop

    override def isErroneous(using Context): Boolean =
      state.typedArgs.tpes.exists(_.isErroneous)

    override def unusableForInference(using Context): Boolean =
      state.typedArgs.exists(_.tpe.unusableForInference)

    override def toString: String = s"FunProto(${args mkString ","} => $resultType)"

    def map(tm: TypeMap)(using Context): FunProto =
      derivedFunProto(args, tm(resultType), typer)

    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T =
      ta(ta.foldOver(x, typedArgs().tpes), resultType)

    override def deepenProto(using Context): FunProto =
      derivedFunProto(args, resultType.deepenProto)

    override def deepenProtoTrans(using Context): FunProto =
      derivedFunProto(args, resultType.deepenProtoTrans, constrainResultDeep = true)

    override def withContext(newCtx: Context): ProtoType =
      if newCtx `eq` protoCtx then this
      else new FunProto(args, resType)(typer, applyKind, state)(using newCtx)
  }

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType, where args are known to be typed
   */
  class FunProtoTyped(args: List[tpd.Tree], resultType: Type)(typer: Typer, applyKind: ApplyKind)(using Context)
  extends FunProto(args, resultType)(typer, applyKind):
    override def typedArgs(norm: (untpd.Tree, Int) => untpd.Tree)(using Context): List[tpd.Tree] = args
    override def typedArg(arg: untpd.Tree, formal: Type)(using Context): tpd.Tree = arg.asInstanceOf[tpd.Tree]
    override def allArgTypesAreCurrent()(using Context): Boolean = true
    override def withContext(ctx: Context): FunProtoTyped = this

  /** A prototype for implicitly inferred views:
   *
   *    []: argType => resultType
   */
  abstract case class ViewProto(argType: Type, resType: Type)
  extends CachedGroundType with ApplyingProto {

    override def resultType(using Context): Type = resType

    def isMatchedBy(tp: Type, keepConstraint: Boolean)(using Context): Boolean =
      ctx.typer.isApplicableType(tp, argType :: Nil, resultType) || {
        resType match {
          case selProto @ SelectionProto(selName: TermName, mbrType, _, _, _) =>
            ctx.typer.hasExtensionMethodNamed(tp, selName, argType, mbrType)
              //.reporting(i"has ext $tp $name $argType $mbrType: $result")
          case _ =>
            false
        }
      }

    def derivedViewProto(argType: Type, resultType: Type)(using Context): ViewProto =
      if ((argType eq this.argType) && (resultType eq this.resultType)) this
      else ViewProto(argType, resultType)

    override def isErroneous(using Context): Boolean =
      argType.isErroneous || resType.isErroneous

    override def unusableForInference(using Context): Boolean =
      argType.unusableForInference || resType.unusableForInference

    def map(tm: TypeMap)(using Context): ViewProto = derivedViewProto(tm(argType), tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T =
      ta(ta(x, argType), resultType)

    override def deepenProto(using Context): ViewProto =
      derivedViewProto(argType, resultType.deepenProto)

    override def deepenProtoTrans(using Context): ViewProto =
      derivedViewProto(argType, resultType.deepenProtoTrans)
  }

  class CachedViewProto(argType: Type, resultType: Type) extends ViewProto(argType, resultType) {
    override def computeHash(bs: Hashable.Binders): Int = doHash(bs, argType, resultType)
    override def eql(that: Type): Boolean = that match
      case that: ViewProto => (argType eq that.argType) && (resType eq that.resType)
      case _ => false
    // equals comes from case class; no need to redefine
  }

  object ViewProto {
    def apply(argType: Type, resultType: Type)(using Context): ViewProto =
      unique(new CachedViewProto(argType, resultType))
  }

  class UnapplyFunProto(argType: Type, typer: Typer)(using Context) extends FunProto(
    untpd.TypedSplice(dummyTreeOfType(argType)(ctx.source)) :: Nil, WildcardType)(typer, applyKind = ApplyKind.Regular)

  /** A prototype for expressions [] that are type-parameterized:
   *
   *    [] [targs] resultType
   */
  case class PolyProto(targs: List[Tree], resType: Type) extends UncachedGroundType with FunOrPolyProto {

    override def resultType(using Context): Type = resType

    def canInstantiate(tp: Type)(using Context) = tp.widen match
      case tp: PolyType => tp.paramNames.length == targs.length
      case _ => false

    override def isMatchedBy(tp: Type, keepConstraint: Boolean)(using Context): Boolean =
      canInstantiate(tp) || tp.member(nme.apply).hasAltWith(d => canInstantiate(d.info))

    def derivedPolyProto(targs: List[Tree], resType: Type): PolyProto =
      if ((targs eq this.targs) && (resType eq this.resType)) this
      else PolyProto(targs, resType)

    override def isErroneous(using Context): Boolean =
      targs.exists(_.tpe.isErroneous)

    override def unusableForInference(using Context): Boolean =
      targs.exists(_.tpe.unusableForInference)

    def map(tm: TypeMap)(using Context): PolyProto =
      derivedPolyProto(targs, tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(using Context): T =
      ta(ta.foldOver(x, targs.tpes), resultType)

    override def deepenProto(using Context): PolyProto =
      derivedPolyProto(targs, resultType.deepenProto)

    override def deepenProtoTrans(using Context): PolyProto =
      derivedPolyProto(targs, resultType.deepenProtoTrans)
  }

  /** A prototype for expressions [] that are known to be functions:
   *
   *    [] _
   */
  @sharable object AnyFunctionProto extends UncachedGroundType with MatchAlways:
    override def toString = "AnyFunctionProto"

  /** A prototype for type constructors that are followed by a type application */
  @sharable object AnyTypeConstructorProto extends UncachedGroundType with MatchAlways:
    override def toString = "AnyTypeConstructorProto"

  extension (pt: Type)
    def isExtensionApplyProto: Boolean = pt match
      case PolyProto(targs, res) => res.isExtensionApplyProto
      case FunProto((arg: untpd.TypedSplice) :: Nil, _) => arg.isExtensionReceiver
      case _ => false

  /** An extractor for Singleton and Precise witness types.
   *
   *       Singleton { type Self = T }     returns Some(T, true)
   *       Precise { type Self = T }       returns Some(T, false)
   */
  object PreciseConstrained:
    def unapply(tp: Type)(using Context): Option[(Type, Boolean)] = tp.dealias match
      case RefinedType(parent, tpnme.Self, TypeAlias(tp)) =>
        val tsym = parent.typeSymbol
        if tsym == defn.SingletonClass then Some((tp, true))
        else if tsym == defn.PreciseClass then Some((tp, false))
        else None
      case _ => None

  /** Add all parameters of given type lambda `tl` to the constraint's domain.
   *  If the constraint contains already some of these parameters in its domain,
   *  make a copy of the type lambda and add the copy's type parameters instead.
   *  Return either the original type lambda, or the copy, if one was made.
   *  Also, if `owningTree` is non-empty or `alwaysAddTypeVars` is true, add a type variable
   *  for each parameter.
   *  @return  The added type lambda, and the list of created type variables.
   */
  def constrained(using Context)(
    tl: TypeLambda, owningTree: untpd.Tree,
    alwaysAddTypeVars: Boolean,
    nestingLevel: Int = ctx.nestingLevel
  ): (TypeLambda, List[TypeVar]) =
    val state = ctx.typerState
    val addTypeVars = alwaysAddTypeVars || !owningTree.isEmpty
    if (tl.isInstanceOf[PolyType])
      assert(!ctx.typerState.isCommittable || addTypeVars,
        s"inconsistent: no typevars were added to committable constraint ${state.constraint}")
      // hk type lambdas can be added to constraints without typevars during match reduction
    val added = state.constraint.ensureFresh(tl)

    def preciseConstrainedRefs(tp: Type, singletonOnly: Boolean): Set[TypeParamRef] = tp match
      case tp: MethodType if tp.isContextualMethod =>
        val ownBounds =
          for
            case PreciseConstrained(ref: TypeParamRef, singleton) <- tp.paramInfos
            if !singletonOnly || singleton
          yield ref
        ownBounds.toSet ++ preciseConstrainedRefs(tp.resType, singletonOnly)
      case tp: LambdaType =>
        preciseConstrainedRefs(tp.resType, singletonOnly)
      case _ =>
        Set.empty

    def newTypeVars: List[TypeVar] =
      val preciseRefs = preciseConstrainedRefs(added, singletonOnly = false)
      for paramRef <- added.paramRefs yield
        val tvar = TypeVar(paramRef, state, nestingLevel, precise = preciseRefs.contains(paramRef))
        state.ownedVars += tvar
        tvar

    val tvars = if addTypeVars then newTypeVars else Nil
    TypeComparer.addToConstraint(added, tvars)
    val singletonRefs = preciseConstrainedRefs(added, singletonOnly = true)
    for paramRef <- added.paramRefs do
      // Constrain all type parameters [T: Singleton] to T <: Singleton
      if singletonRefs.contains(paramRef) then paramRef <:< defn.SingletonType
    (added, tvars)
  end constrained

  def constrained(tl: TypeLambda, owningTree: untpd.Tree)(using Context): (TypeLambda, List[TypeVar]) =
    constrained(tl, owningTree,
      alwaysAddTypeVars = tl.isInstanceOf[PolyType] && ctx.typerState.isCommittable)

  /**  Same as `constrained(tl, EmptyTree, alwaysAddTypeVars = true)`, but returns just the created type vars. */
  def constrained(tl: TypeLambda)(using Context): List[TypeVar] =
    constrained(tl, EmptyTree, alwaysAddTypeVars = true)._2

  /** Instantiate `tl` with fresh type variables added to the constraint. */
  def instantiateWithTypeVars(tl: TypeLambda)(using Context): Type =
    val tvars = constrained(tl)
    tl.instantiate(tvars)

  /** A fresh type variable added to the current constraint.
   *  @param  bounds        The initial bounds of the variable
   *  @param  name          The name of the variable, defaults a fresh `DepParamName`
   *  @param  nestingLevel  See `TypeVar#nestingLevel`
   *  @param  represents    If it exists, a ParamRef that this TypeVar represents,
   *                        to be retrieved using `representedParamRef`.
   *                        in the substitution generated by `resultTypeApprox`
   *  If `represents` exists, it is stored in the result type of the PolyType
   *  that backs the TypeVar, to be retrieved by `representedParamRef`.
   */
  def newTypeVar(using Context)(
      bounds: TypeBounds, name: TypeName = DepParamName.fresh().toTypeName,
      nestingLevel: Int = ctx.nestingLevel, represents: Type = NoType): TypeVar =
    val poly = PolyType(name :: Nil)(
        pt => bounds :: Nil,
        pt => represents.orElse(defn.AnyType))
    constrained(poly, untpd.EmptyTree, alwaysAddTypeVars = true, nestingLevel)
      ._2.head

  /** If `param` was created using `newTypeVar(..., represents = X)`, returns X.
   *  This is used in:
   *  - `Inferencing#constrainIfDependentParamRef` to retrieve the dependent function
   *    parameter for which the variable was substituted.
   *  - `ConstraintHandling#LevelAvoidMap#legalVar` to retrieve the type variable that was
   *    avoided in a previous call to `legalVar`.
   */
  def representedParamRef(param: TypeParamRef)(using Context): Type =
    param.binder.resultType match
      case ref: ParamRef => ref
      case _ => NoType

  /** Create a new TypeVar that represents a dependent method parameter singleton `ref` */
  def newDepTypeVar(ref: TermParamRef)(using Context): TypeVar =
    newTypeVar(
      TypeBounds.upper(AndType(ref.underlying.widenExpr, defn.SingletonClass.typeRef)),
      represents = ref)

  /** The result type of `mt`, where all references to parameters of `mt` are
   *  replaced by either wildcards or TypeParamRefs.
   */
  def resultTypeApprox(mt: MethodType, wildcardOnly: Boolean = false)(using Context): Type =
    if mt.isResultDependent then
      def replacement(ref: TermParamRef) =
        if wildcardOnly
           || ctx.mode.is(Mode.TypevarsMissContext)
           || !ref.underlying.widenExpr.isValueTypeOrWildcard
        then
          WildcardType(ref.underlying.substParams(mt, mt.paramRefs.map(_ => WildcardType)).toBounds)
        else
          newDepTypeVar(ref)
      mt.resultType.substParams(mt, mt.paramRefs.map(replacement))
    else mt.resultType

  /** The normalized form of a type
   *   - instantiate polymorphic types with fresh type variables in the current constraint
   *   - skips implicit parameters of methods and functions;
   *     if result type depends on implicit parameter, replace with wildcard.
   *   - converts non-dependent method types to the corresponding function types
   *     unless the expected type is an ApplyingProto or IgnoredProto.
   *   - dereferences parameterless method types
   *   - dereferences nullary method types provided the corresponding function type
   *     is not a subtype of the expected type.
   * Note: We need to take account of the possibility of inserting a () argument list in normalization. Otherwise, a type with a
   *     def toString(): String
   * member would not count as a valid solution for ?{toString: String}. This would then lead to an implicit
   * insertion, with a nice explosion of inference search because of course every implicit result has some sort
   * of toString method. The problem is solved by dereferencing nullary method types if the corresponding
   * function type is not compatible with the prototype.
   */
  def normalize(tp: Type, pt: Type, followIFT: Boolean = true)(using Context): Type = {
    Stats.record("normalize")
    tp.widenSingleton match {
      case poly: PolyType =>
        normalize(instantiateWithTypeVars(poly), pt)
      case mt: MethodType =>
        if (mt.isImplicitMethod) normalize(resultTypeApprox(mt, wildcardOnly = true), pt)
        else if (mt.isResultDependent) tp
        else {
          val rt = normalize(mt.resultType, pt)
          pt match {
            case pt: IgnoredProto  =>
              tp
            case pt: ApplyingProto =>
              if (rt eq mt.resultType) tp
              else mt.derivedLambdaType(mt.paramNames, mt.paramInfos, rt)
            case _ =>
              val ft = defn.FunctionOf(mt.paramInfos, rt)
              if mt.paramInfos.nonEmpty || (ft frozen_<:< pt) then ft else rt
          }
        }
      case et: ExprType =>
        normalize(et.resultType, pt)
      case wtp =>
        val iftp = defn.asContextFunctionType(wtp)
        if iftp.exists && followIFT then normalize(iftp.functionArgInfos.last, pt)
        else tp
    }
  }

  /** Approximate occurrences of parameter types and uninstantiated typevars
   *  by wildcard types.
   */
  private def wildApprox(tp: Type, theMap: WildApproxMap | Null, seen: Set[TypeParamRef], internal: Set[TypeLambda])(using Context): Type =
    tp match {
    case tp: NamedType => // default case, inlined for speed
      val isPatternBoundTypeRef = tp.isInstanceOf[TypeRef] && tp.symbol.isPatternBound
      if (isPatternBoundTypeRef) WildcardType(tp.underlying.bounds)
      else if (tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) tp
      else tp.derivedSelect(wildApprox(tp.prefix, theMap, seen, internal))
    case tp @ AppliedType(tycon, args) =>
      def wildArgs = args.mapConserve(arg => wildApprox(arg, theMap, seen, internal))
      wildApprox(tycon, theMap, seen, internal) match {
        case WildcardType(TypeBounds(lo, hi)) if hi.typeParams.hasSameLengthAs(args) =>
          val args1 = wildArgs
          val lo1 = if lo.typeParams.hasSameLengthAs(args) then lo.appliedTo(args1) else lo
          WildcardType(TypeBounds(lo1, hi.appliedTo(args1)))
        case WildcardType(_) =>
          WildcardType
        case tycon1 => tp.derivedAppliedType(tycon1, wildArgs)
      }
    case tp: RefinedType => // default case, inlined for speed
      tp.derivedRefinedType(
          wildApprox(tp.parent, theMap, seen, internal),
          tp.refinedName,
          wildApprox(tp.refinedInfo, theMap, seen, internal))
    case tp: AliasingBounds => // default case, inlined for speed
      tp.derivedAlias(wildApprox(tp.alias, theMap, seen, internal))
    case tp: TypeBounds =>
      tp.derivedTypeBounds(
        wildApprox(tp.lo, theMap, seen, internal),
        wildApprox(tp.hi, theMap, seen, internal))
    case tp @ TypeParamRef(tl, _) if internal.contains(tl) => tp
    case tp @ TypeParamRef(poly, pnum) =>
      def wildApproxBounds(bounds: TypeBounds) =
        if (seen.contains(tp)) WildcardType
        else WildcardType(wildApprox(bounds, theMap, seen + tp, internal).bounds)
      def unconstrainedApprox = wildApproxBounds(poly.paramInfos(pnum))
      def approxPoly =
        if (ctx.mode.is(Mode.TypevarsMissContext)) unconstrainedApprox
        else
          ctx.typerState.constraint.entry(tp) match {
            case bounds: TypeBounds => wildApproxBounds(bounds)
            case NoType             => unconstrainedApprox
            case inst               => wildApprox(inst, theMap, seen, internal)
          }
      approxPoly
    case TermParamRef(mt, pnum) =>
      WildcardType(TypeBounds.upper(wildApprox(mt.paramInfos(pnum), theMap, seen, internal)))
    case tp: TypeVar =>
      wildApprox(tp.underlying, theMap, seen, internal)
    case tp: AndType =>
      def approxAnd = {
        val tp1a = wildApprox(tp.tp1, theMap, seen, internal)
        val tp2a = wildApprox(tp.tp2, theMap, seen, internal)
        def wildBounds(tp: Type) =
          if (tp.isInstanceOf[WildcardType]) tp.bounds else TypeBounds.upper(tp)
        if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
          WildcardType(wildBounds(tp1a) & wildBounds(tp2a))
        else
          tp.derivedAndType(tp1a, tp2a)
      }
      approxAnd
    case tp: OrType =>
      def approxOr = {
        val tp1a = wildApprox(tp.tp1, theMap, seen, internal)
        val tp2a = wildApprox(tp.tp2, theMap, seen, internal)
        if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
          WildcardType(tp1a.bounds | tp2a.bounds)
        else
          tp.derivedOrType(tp1a, tp2a)
      }
      approxOr
    case tp: SelectionProto =>
      tp.derivedSelectionProto(tp.name, wildApprox(tp.memberProto, theMap, seen, internal), NoViewsAllowed, tp.nameSpan)
    case tp: ViewProto =>
      tp.derivedViewProto(
          wildApprox(tp.argType, theMap, seen, internal),
          wildApprox(tp.resultType, theMap, seen, internal))
    case tp: FunProto =>
      val args = tp.args.mapconserve(arg =>
        val argTp = tp.typeOfArg(arg) match
          case NoType => WildcardType
          case tp => wildApprox(tp, theMap, seen, internal)
        arg.withType(argTp))
      val resTp = wildApprox(tp.resultType, theMap, seen, internal)
      if (args eq tp.args) && (resTp eq tp.resultType) then
        tp
      else
        FunProtoTyped(args, resTp)(ctx.typer, tp.applyKind)
    case tp: IgnoredProto =>
      WildcardType
    case  _: ThisType | _: BoundType => // default case, inlined for speed
      tp
    case tl: TypeLambda =>
      val internal1 = internal + tl
      tl.derivedLambdaType(
        paramInfos = tl.paramInfos.mapConserve(wildApprox(_, theMap, seen, internal1).bounds),
        resType = wildApprox(tl.resType, theMap, seen, internal1)
      )
    case _ =>
      (if (theMap != null && seen.eq(theMap.seen)) theMap else new WildApproxMap(seen, internal))
        .mapOver(tp)
  }

  final def wildApprox(tp: Type)(using Context): Type = wildApprox(tp, null, Set.empty, Set.empty)

  @sharable object LhsProto extends UncachedGroundType with MatchAlways

  private[ProtoTypes] class WildApproxMap(val seen: Set[TypeParamRef], val internal: Set[TypeLambda])(using Context) extends TypeMap {
    def apply(tp: Type): Type = wildApprox(tp, this, seen, internal)
  }

  /** Dummy tree to be used as an argument of a FunProto or ViewProto type */
  object dummyTreeOfType {
    def apply(tp: Type)(implicit src: SourceFile): Tree =
      untpd.Literal(Constant(null)) withTypeUnchecked tp
    def unapply(tree: untpd.Tree): Option[Type] = untpd.unsplice(tree) match {
      case tree @ Literal(Constant(null)) => Some(tree.typeOpt)
      case _ => None
    }
  }

  private val sameTree = (t: untpd.Tree, n: Int) => t
}
