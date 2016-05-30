package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import Trees._
import Constants._
import Scopes._
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import util.common._
import Decorators._
import Uniques._
import ErrorReporting.errorType
import config.Printers._
import collection.mutable

object ProtoTypes {

  import tpd._

  /** A trait defining an `isCompatible` method. */
  trait Compatibility {

    /** Is there an implicit conversion from `tp` to `pt`? */
    def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean

    /** A type `tp` is compatible with a type `pt` if one of the following holds:
     *    1. `tp` is a subtype of `pt`
     *    2. `pt` is by name parameter type, and `tp` is compatible with its underlying type
     *    3. there is an implicit conversion from `tp` to `pt`.
     *    4. `tp` is a numeric subtype of `pt` (this case applies even if implicit conversions are disabled)
     */
    def isCompatible(tp: Type, pt: Type)(implicit ctx: Context): Boolean =
      (tp.widenExpr relaxed_<:< pt.widenExpr) || viewExists(tp, pt)

    /** Test compatibility after normalization in a fresh typerstate. */
    def normalizedCompatible(tp: Type, pt: Type)(implicit ctx: Context) = {
      val nestedCtx = ctx.fresh.setExploreTyperState
      isCompatible(normalize(tp, pt)(nestedCtx), pt)(nestedCtx)
    }

    /** Check that the result type of the current method
     *  fits the given expected result type.
     */
    def constrainResult(mt: Type, pt: Type)(implicit ctx: Context): Boolean = pt match {
      case pt: FunProto =>
        mt match {
          case mt: MethodType =>
            mt.isDependent || constrainResult(mt.resultType, pt.resultType)
          case _ =>
            true
        }
      case _: ValueTypeOrProto if !(pt isRef defn.UnitClass) =>
        mt match {
          case mt: MethodType =>
            mt.isDependent || isCompatible(normalize(mt, pt), pt)
          case _ =>
            isCompatible(mt, pt)
        }
      case _: WildcardType =>
        isCompatible(mt, pt)
      case _ =>
        true
    }
  }

  object NoViewsAllowed extends Compatibility {
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean = false
  }

  /** A trait for prototypes that match all types */
  trait MatchAlways extends ProtoType {
    def isMatchedBy(tp1: Type)(implicit ctx: Context) = true
    def map(tm: TypeMap)(implicit ctx: Context): ProtoType = this
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T = x
  }

  /** A class marking ignored prototypes that can be revealed by `deepenProto` */
  case class IgnoredProto(ignored: Type) extends UncachedGroundType with MatchAlways {
    override def deepenProto(implicit ctx: Context): Type = ignored
  }

  /** A prototype for expressions [] that are part of a selection operation:
   *
   *       [ ].name: proto
   */
  abstract case class SelectionProto(val name: Name, val memberProto: Type, val compat: Compatibility)
  extends CachedProxyType with ProtoType with ValueTypeOrProto {

    override def isMatchedBy(tp1: Type)(implicit ctx: Context) = {
      name == nme.WILDCARD || {
        val mbr = tp1.member(name)
        def qualifies(m: SingleDenotation) =
          memberProto.isRef(defn.UnitClass) ||
          compat.normalizedCompatible(m.info, memberProto)
        mbr match { // hasAltWith inlined for performance
          case mbr: SingleDenotation => mbr.exists && qualifies(mbr)
          case _ => mbr hasAltWith qualifies
        }
      }
    }

    def underlying(implicit ctx: Context) = WildcardType

    def derivedSelectionProto(name: Name, memberProto: Type, compat: Compatibility)(implicit ctx: Context) =
      if ((name eq this.name) && (memberProto eq this.memberProto) && (compat eq this.compat)) this
      else SelectionProto(name, memberProto, compat)

    override def equals(that: Any): Boolean = that match {
      case that: SelectionProto =>
        (name eq that.name) && (memberProto == that.memberProto) && (compat eq that.compat)
      case _ =>
        false
    }

    def map(tm: TypeMap)(implicit ctx: Context) = derivedSelectionProto(name, tm(memberProto), compat)
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context) = ta(x, memberProto)

    override def deepenProto(implicit ctx: Context) = derivedSelectionProto(name, memberProto.deepenProto, compat)

    override def computeHash = addDelta(doHash(name, memberProto), if (compat eq NoViewsAllowed) 1 else 0)
  }

  class CachedSelectionProto(name: Name, memberProto: Type, compat: Compatibility) extends SelectionProto(name, memberProto, compat)

  object SelectionProto {
    def apply(name: Name, memberProto: Type, compat: Compatibility)(implicit ctx: Context): SelectionProto = {
      val selproto = new CachedSelectionProto(name, memberProto, compat)
      if (compat eq NoViewsAllowed) unique(selproto) else selproto
    }
  }

  /** Create a selection proto-type, but only one level deep;
   *  treat constructors specially
   */
  def selectionProto(name: Name, tp: Type, typer: Typer)(implicit ctx: Context) =
    if (name.isConstructorName) WildcardType
    else tp match {
      case tp: UnapplyFunProto => new UnapplySelectionProto(name)
      case tp => SelectionProto(name, IgnoredProto(tp), typer)
    }

  /** A prototype for expressions [] that are in some unspecified selection operation
   *
   *    [].?: ?
   *
   *  Used to indicate that expression is in a context where the only valid
   *  operation is further selection. In this case, the expression need not be a value.
   *  @see checkValue
   */
  @sharable object AnySelectionProto extends SelectionProto(nme.WILDCARD, WildcardType, NoViewsAllowed)

  /** A prototype for selections in pattern constructors */
  class UnapplySelectionProto(name: Name) extends SelectionProto(name, WildcardType, NoViewsAllowed)

  trait ApplyingProto extends ProtoType

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType
   */
  case class FunProto(args: List[untpd.Tree], resType: Type, typer: Typer)(implicit ctx: Context)
  extends UncachedGroundType with ApplyingProto {
    private var myTypedArgs: List[Tree] = Nil

    override def resultType(implicit ctx: Context) = resType

    /** A map in which typed arguments can be stored to be later integrated in `typedArgs`. */
    private var myTypedArg: SimpleMap[untpd.Tree, Tree] = SimpleMap.Empty

    def isMatchedBy(tp: Type)(implicit ctx: Context) =
      typer.isApplicable(tp, Nil, typedArgs, resultType)

    def derivedFunProto(args: List[untpd.Tree], resultType: Type, typer: Typer) =
      if ((args eq this.args) && (resultType eq this.resultType) && (typer eq this.typer)) this
      else new FunProto(args, resultType, typer)

    def argsAreTyped: Boolean = myTypedArgs.size == args.length

    /** The typed arguments. This takes any arguments already typed using
     *  `typedArg` into account.
     */
    def typedArgs: List[Tree] = {
      if (!argsAreTyped)
        myTypedArgs = args mapconserve { arg =>
          val targ = myTypedArg(arg)
          if (targ != null) targ else typer.typed(arg)
        }
      myTypedArgs
    }

    /** Type single argument and remember the unadapted result in `myTypedArg`.
     *  used to avoid repeated typings of trees when backtracking.
     */
    def typedArg(arg: untpd.Tree, formal: Type)(implicit ctx: Context): Tree = {
      var targ = myTypedArg(arg)
      if (targ == null) {
        targ = typer.typedUnadapted(arg, formal)
        if (!ctx.reporter.hasPending) myTypedArg = myTypedArg.updated(arg, targ)
      }
      typer.adapt(targ, formal, arg)
    }

    private var myTupled: Type = NoType

    /** The same proto-type but with all arguments combined in a single tuple */
    def tupled: FunProto = myTupled match {
      case pt: FunProto =>
        pt
      case _ =>
        myTupled = new FunProto(untpd.Tuple(args) :: Nil, resultType, typer)
        tupled
    }

    /** Somebody called the `tupled` method of this prototype */
    def isTupled: Boolean = myTupled.isInstanceOf[FunProto]

    override def toString = s"FunProto(${args mkString ","} => $resultType)"

    def map(tm: TypeMap)(implicit ctx: Context): FunProto =
      derivedFunProto(args, tm(resultType), typer)

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta.foldOver(x, typedArgs.tpes), resultType)

    override def deepenProto(implicit ctx: Context) = derivedFunProto(args, resultType.deepenProto, typer)
  }


  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType, where args are known to be typed
   */
  class FunProtoTyped(args: List[tpd.Tree], resultType: Type, typer: Typer)(implicit ctx: Context) extends FunProto(args, resultType, typer)(ctx) {
    override def typedArgs = args
    override def argsAreTyped = true
  }

  /** A prototype for implicitly inferred views:
   *
   *    []: argType => resultType
   */
  abstract case class ViewProto(argType: Type, resType: Type)
  extends CachedGroundType with ApplyingProto {

    override def resultType(implicit ctx: Context) = resType

    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean =
      ctx.typer.isApplicable(tp, argType :: Nil, resultType)

    def derivedViewProto(argType: Type, resultType: Type)(implicit ctx: Context) =
      if ((argType eq this.argType) && (resultType eq this.resultType)) this
      else ViewProto(argType, resultType)

    def map(tm: TypeMap)(implicit ctx: Context): ViewProto = derivedViewProto(tm(argType), tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta(x, argType), resultType)

    override def deepenProto(implicit ctx: Context) = derivedViewProto(argType, resultType.deepenProto)
  }

  class CachedViewProto(argType: Type, resultType: Type) extends ViewProto(argType, resultType) {
    override def computeHash = doHash(argType, resultType)
  }

  object ViewProto {
    def apply(argType: Type, resultType: Type)(implicit ctx: Context) =
      unique(new CachedViewProto(argType, resultType))
  }

  class UnapplyFunProto(argType: Type, typer: Typer)(implicit ctx: Context) extends FunProto(
    untpd.TypedSplice(dummyTreeOfType(argType)) :: Nil, WildcardType, typer)

  /** A prototype for expressions [] that are type-parameterized:
   *
   *    [] [targs] resultType
   */
  case class PolyProto(targs: List[Type], resType: Type) extends UncachedGroundType with ProtoType {

    override def resultType(implicit ctx: Context) = resType

    override def isMatchedBy(tp: Type)(implicit ctx: Context) = {
      def isInstantiatable(tp: Type) = tp.widen match {
        case PolyType(paramNames) => paramNames.length == targs.length
        case _ => false
      }
      isInstantiatable(tp) || tp.member(nme.apply).hasAltWith(d => isInstantiatable(d.info))
    }

    def derivedPolyProto(targs: List[Type], resultType: Type) =
      if ((targs eq this.targs) && (resType eq this.resType)) this
      else PolyProto(targs, resType)

    def map(tm: TypeMap)(implicit ctx: Context): PolyProto =
      derivedPolyProto(targs mapConserve tm, tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta.foldOver(x, targs), resultType)

    override def deepenProto(implicit ctx: Context) = derivedPolyProto(targs, resultType.deepenProto)
  }

  /** A prototype for expressions [] that are known to be functions:
   *
   *    [] _
   */
  @sharable object AnyFunctionProto extends UncachedGroundType with MatchAlways

  /** Add all parameters in given polytype `pt` to the constraint's domain.
   *  If the constraint contains already some of these parameters in its domain,
   *  make a copy of the polytype and add the copy's type parameters instead.
   *  Return either the original polytype, or the copy, if one was made.
   *  Also, if `owningTree` is non-empty, add a type variable for each parameter.
   *  @return  The added polytype, and the list of created type variables.
   */
  def constrained(pt: PolyType, owningTree: untpd.Tree)(implicit ctx: Context): (PolyType, List[TypeVar]) = {
    val state = ctx.typerState
    assert(!(ctx.typerState.isCommittable && owningTree.isEmpty),
      s"inconsistent: no typevars were added to committable constraint ${state.constraint}")

    def newTypeVars(pt: PolyType): List[TypeVar] =
      for (n <- (0 until pt.paramNames.length).toList)
      yield new TypeVar(PolyParam(pt, n), state, owningTree, ctx.owner)

    val added =
      if (state.constraint contains pt) pt.duplicate(pt.paramNames, pt.paramBounds, pt.resultType)
      else pt
    val tvars = if (owningTree.isEmpty) Nil else newTypeVars(added)
    ctx.typeComparer.addToConstraint(added, tvars)
    (added, tvars)
  }

  /**  Same as `constrained(pt, EmptyTree)`, but returns just the created polytype */
  def constrained(pt: PolyType)(implicit ctx: Context): PolyType = constrained(pt, EmptyTree)._1

  /** The normalized form of a type
   *   - unwraps polymorphic types, tracking their parameters in the current constraint
   *   - skips implicit parameters; if result type depends on implicit parameter,
   *     replace with Wildcard.
   *   - converts non-dependent method types to the corresponding function types
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
  def normalize(tp: Type, pt: Type)(implicit ctx: Context): Type = Stats.track("normalize") {
    tp.widenSingleton match {
      case poly: PolyType => normalize(constrained(poly).resultType, pt)
      case mt: MethodType =>
        if (mt.isImplicit)
          if (mt.isDependent)
            mt.resultType.substParams(mt, mt.paramTypes.map(Function.const(WildcardType)))
          else mt.resultType
        else
          if (mt.isDependent) tp
          else {
            val rt = normalize(mt.resultType, pt)
            if (pt.isInstanceOf[ApplyingProto])
              mt.derivedMethodType(mt.paramNames, mt.paramTypes, rt)
            else {
              val ft = defn.FunctionOf(mt.paramTypes, rt)
              if (mt.paramTypes.nonEmpty || ft <:< pt) ft else rt
            }
          }
      case et: ExprType => et.resultType
      case _ => tp
    }
  }

  /** Approximate occurrences of parameter types and uninstantiated typevars
   *  by wildcard types.
   */
  final def wildApprox(tp: Type, theMap: WildApproxMap = null)(implicit ctx: Context): Type = tp match {
    case tp: NamedType => // default case, inlined for speed
      if (tp.symbol.isStatic) tp
      else tp.derivedSelect(wildApprox(tp.prefix, theMap))
    case tp: RefinedType => // default case, inlined for speed
      tp.derivedRefinedType(wildApprox(tp.parent, theMap), tp.refinedName, wildApprox(tp.refinedInfo, theMap))
    case tp: TypeAlias => // default case, inlined for speed
      tp.derivedTypeAlias(wildApprox(tp.alias, theMap))
    case tp @ PolyParam(poly, pnum) =>
      def unconstrainedApprox = WildcardType(wildApprox(poly.paramBounds(pnum)).bounds)
      if (ctx.mode.is(Mode.TypevarsMissContext))
        unconstrainedApprox
      else
        ctx.typerState.constraint.entry(tp) match {
          case bounds: TypeBounds => wildApprox(WildcardType(bounds))
          case NoType => unconstrainedApprox
          case inst => wildApprox(inst)
        }
    case MethodParam(mt, pnum) =>
      WildcardType(TypeBounds.upper(wildApprox(mt.paramTypes(pnum))))
    case tp: TypeVar =>
      wildApprox(tp.underlying)
    case tp: AndType =>
      val tp1a = wildApprox(tp.tp1)
      val tp2a = wildApprox(tp.tp2)
      def wildBounds(tp: Type) =
        if (tp.isInstanceOf[WildcardType]) tp.bounds else TypeBounds.upper(tp)
      if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
        WildcardType(wildBounds(tp1a) & wildBounds(tp2a))
      else
        tp.derivedAndType(tp1a, tp2a)
    case tp: OrType =>
      val tp1a = wildApprox(tp.tp1)
      val tp2a = wildApprox(tp.tp2)
      if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
        WildcardType(tp1a.bounds | tp2a.bounds)
      else
        tp.derivedOrType(tp1a, tp2a)
    case tp: LazyRef =>
      WildcardType
    case tp: SelectionProto =>
      tp.derivedSelectionProto(tp.name, wildApprox(tp.memberProto), NoViewsAllowed)
    case tp: ViewProto =>
      tp.derivedViewProto(wildApprox(tp.argType), wildApprox(tp.resultType))
    case  _: ThisType | _: BoundType | NoPrefix => // default case, inlined for speed
      tp
    case _ =>
      (if (theMap != null) theMap else new WildApproxMap).mapOver(tp)
  }

  @sharable object AssignProto extends UncachedGroundType with MatchAlways

  private[ProtoTypes] class WildApproxMap(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = wildApprox(tp, this)
  }

  @sharable private lazy val dummyTree = untpd.Literal(Constant(null))

  /** Dummy tree to be used as an argument of a FunProto or ViewProto type */
  object dummyTreeOfType {
    def apply(tp: Type): Tree = dummyTree withTypeUnchecked tp
    def unapply(tree: Tree): Option[Type] = tree match {
      case Literal(Constant(null)) => Some(tree.typeOpt)
      case _ => None
    }
  }
}
