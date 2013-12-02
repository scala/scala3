package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import Trees._
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import Decorators._
import ErrorReporting.{errorType, InfoString}
import collection.mutable.ListBuffer

object Inferencing {

  import tpd._

  /** A trait defining an `isCompatible` method. */
  trait Compatibility {

    /** Is there an implicit conversion from `tp` to `pt`? */
    def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean

    /** A type `tp` is compatible with a type `pt` if one of the following holds:
     *    1. `tp` is a subtype of `pt`
     *    2. `pt` is by name parameter type, and `tp` is compatible with its underlying type
     *    3. there is an implicit conversion from `tp` to `pt`.
     */
    def isCompatible(tp: Type, pt: Type)(implicit ctx: Context): Boolean = {
      def skipByName(tp: Type): Type =
        if (tp isRef defn.ByNameParamClass) tp.typeArgs.head else tp
      skipByName(tp) <:< skipByName(pt) || viewExists(tp, pt)
    }

    /** Test compatibility after normalization in a fresh typerstate */
    def normalizedCompatible(tp: Type, pt: Type)(implicit ctx: Context) = {
      val nestedCtx = ctx.fresh.withExploreTyperState
      isCompatible(normalize(tp)(nestedCtx), pt)(nestedCtx)
    }
  }

  /** A prototype for expressions [] that are part of a selection operation:
   *
   *       [ ].name: proto
   */
  class SelectionProto(name: Name, proto: Type)
  extends RefinedType(WildcardType, name)(_ => proto) with ProtoType with Compatibility {
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean = false
    override def isMatchedBy(tp1: Type)(implicit ctx: Context) =
      name == nme.WILDCARD || {
        val mbr = tp1.member(name)
        mbr.exists && mbr.hasAltWith(m => normalizedCompatible(m.info, proto))
      }
    override def toString = "Proto" + super.toString
  }

  /** Create a selection proto-type, but only one level deep;
   *  treat constructors specially
   */
  def selectionProto(name: Name, tp: Type) =
    if (name.isConstructorName) WildcardType
    else {
      val rtp = tp match {
        case tp: ProtoType => WildcardType
        case _ => tp
      }
      new SelectionProto(name, rtp)
    }

  /** A prototype for expressions [] that are in some unspecified selection operation
   *
   *    [].?: ?
   *
   *  Used to indicate that expression is in a context where the only valid
   *  operation is further selection. In this case, the expression need not be a value.
   *  @see checkValue
   */
  object AnySelectionProto extends SelectionProto(nme.WILDCARD, WildcardType)

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType
   */
  case class FunProto(args: List[untpd.Tree], override val resultType: Type, typer: Typer)(implicit ctx: Context)
  extends UncachedGroundType with ProtoType {
    private var myTypedArgs: List[Tree] = Nil

    /** A map in which typed arguments can be stored to be later integrated in `typedArgs`. */
    private var myTypedArg: SimpleMap[untpd.Tree, Tree] = SimpleMap.Empty

    def isMatchedBy(tp: Type)(implicit ctx: Context) =
      typer.isApplicable(tp, typedArgs, resultType)

    def argsAreTyped: Boolean = myTypedArgs.nonEmpty || args.isEmpty

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
     *  used to avoid repreated typings of trees when backtracking.
     */
    def typedArg(arg: untpd.Tree, formal: Type)(implicit ctx: Context): Tree = {
      var targ = myTypedArg(arg)
      if (targ == null) {
        targ = typer.typedUnadapted(arg, formal)
        myTypedArg = myTypedArg.updated(arg, targ)
      }
      typer.adapt(targ, formal)
    }

    override def toString = s"FunProto(${args mkString ","} => $resultType)"
  }

  /** A prototype for implicitly inferred views:
   *
   *    []: argType => resultType
   */
  case class ViewProto(argType: Type, override val resultType: Type)(implicit ctx: Context)
  extends CachedGroundType with ProtoType {
    def isMatchedBy(tp: Type)(implicit ctx: Context) =
      ctx.typer.isApplicable(tp, argType :: Nil, resultType)
    override def namedPartsWith(p: NamedType => Boolean)(implicit ctx: Context): collection.Set[NamedType] =
      AndType(argType, resultType).namedPartsWith(p) // this is more efficient than oring two namedParts sets
    override def computeHash = doHash(argType, resultType)
  }

  /** A prototype for expressions [] that are type-parameterized:
   *
   *    [] [?_, ..., ?_nargs] resultType
   */
  case class PolyProto(nargs: Int, override val resultType: Type) extends UncachedGroundType

  /** A prototype for expressions [] that are known to be functions:
   *
   *    [] _
   */
  object AnyFunctionProto extends UncachedGroundType with ProtoType {
    def isMatchedBy(tp: Type)(implicit ctx: Context) = true
  }

  /** The normalized form of a type
   *   - unwraps polymorphic types, tracking their parameters in the current constraint
   *   - skips implicit parameters
   *   - converts non-dependent method types to the corresponding function types
   *   - dereferences parameterless method types
   */
  def normalize(tp: Type)(implicit ctx: Context): Type = Stats.track("normalize") {
    tp.widenSingleton match {
      case pt: PolyType => normalize(constrained(pt).resultType)
      case mt: MethodType if !mt.isDependent =>
        if (mt.isImplicit) mt.resultType
        else defn.FunctionType(mt.paramTypes, mt.resultType)
      case et: ExprType => et.resultType
      case _ => tp
    }
  }

  /** An enumeration controlling the degree of forcing in "is-dully-defined" checks. */
  object ForceDegree extends Enumeration {
    val none,           // don't force type variables
        noBottom,       // force type variables, fail if forced to Nothing or Null
        all = Value     // force type variables, don't fail
  }

  /** Is type fully defined, meaning the type does not contain wildcard types
   *  or uninstantiated type variables. As a side effect, this will minimize
   *  any uninstantiated type variables, according to the given force degree,
   *  but only if the overall result of `isFullyDefined` is `true`.
   *  Variables that are successfully minimized do not count as uninstantiated.
   */
  def isFullyDefined(tp: Type, force: ForceDegree.Value)(implicit ctx: Context): Boolean = {
    val nestedCtx = ctx.fresh.withNewTyperState
    val result = new IsFullyDefinedAccumulator(force)(nestedCtx).traverse(tp)
    if (result) nestedCtx.typerState.commit()
    result
  }

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, pos: Position)(implicit ctx: Context) =
    if (isFullyDefined(tp, ForceDegree.all)) tp
    else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $pos") // !!! DEBUG

  private class IsFullyDefinedAccumulator(force: ForceDegree.Value)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    def traverse(tp: Type): Boolean = apply(true, tp)
    def apply(x: Boolean, tp: Type) = !x || isOK(tp) && foldOver(x, tp)
    def isOK(tp: Type): Boolean = tp match {
      case _: WildcardType =>
        false
      case tvar: TypeVar if force != ForceDegree.none && !tvar.isInstantiated =>
        val inst = tvar.instantiate(fromBelow = true)
        println(i"forced instantiation of ${tvar.origin} = $inst")
        (force == ForceDegree.all || inst != defn.NothingType && inst != defn.NullType) && traverse(inst)
      case _ =>
        true
    }
  }

  /** Recursively and also follow type declarations and type aliases. */
  def widenForMatchSelector(tp: Type)(implicit ctx: Context): Type = tp.widen match {
    case tp: TypeRef if !tp.symbol.isClass => widenForMatchSelector(tp.bounds.hi)
    case tp => tp
  }

  /** Check that type arguments `args` conform to corresponding bounds in `poly` */
  def checkBounds(args: List[tpd.Tree], poly: PolyType, pos: Position)(implicit ctx: Context): Unit =
    for ((arg, bounds) <- args zip poly.paramBounds) {
      def notConforms(which: String, bound: Type) =
        ctx.error(i"Type argument ${arg.tpe} does not conform to $which bound $bound", arg.pos)
      if (!(arg.tpe <:< bounds.hi)) notConforms("upper", bounds.hi)
      if (!(bounds.lo <:< arg.tpe)) notConforms("lower", bounds.lo)
    }

  /** Check that type `tp` is stable.
   *  @return The type itself
   */
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Type = {
    if (!tp.isStable) ctx.error(i"Prefix $tp is not stable", pos)
    tp
  }

  /** Check that `tp` is a class type with a stable prefix.
   *  @return  Underlying class symbol if type checks out OK, ObjectClass if not.
   */
  def checkClassTypeWithStablePrefix(tp: Type, pos: Position)(implicit ctx: Context): ClassSymbol = tp.dealias match {
    case tp: TypeRef if tp.symbol.isClass =>
      checkStable(tp.prefix, pos)
      tp.symbol.asClass
    case _: TypeVar | _: AnnotatedType =>
      checkClassTypeWithStablePrefix(tp.asInstanceOf[TypeProxy].underlying, pos)
    case _ =>
      ctx.error(i"$tp is not a class type", pos)
      defn.ObjectClass
  }

  def checkInstantiatable(cls: ClassSymbol, pos: Position): Unit = {
    ??? // to be done in later phase: check that class `cls` is legal in a new.
  }

  /** Add all parameters in given polytype `pt` to the constraint's domain.
   *  If the constraint contains already some of these parameters in its domain,
   *  make a copy of the polytype and add the copy's type parameters instead.
   *  Return either the original polytype, or the copy, if one was made.
   *  Also, if `owningTree` is non-empty, add a type variable for each parameter.
   *  @return  The added polytype, and the list of created type variables.
   */
  def constrained(pt: PolyType, owningTree: untpd.Tree)(implicit ctx: Context): (PolyType, List[TypeVar]) = {
    val state = ctx.typerState
    def howmany = if (owningTree.isEmpty) "no" else "some"
    def committable = if (ctx.typerState.isCommittable) "committable" else "uncommittable"
    assert(owningTree.isEmpty != ctx.typerState.isCommittable,
      s"inconsistent: $howmany typevars were added to $committable constraint ${state.constraint}")

    def newTypeVars(pt: PolyType): List[TypeVar] =
      for (n <- (0 until pt.paramNames.length).toList)
      yield new TypeVar(PolyParam(pt, n), state, owningTree)

    val added =
      if (state.constraint contains pt) pt.copy(pt.paramNames, pt.paramBounds, pt.resultType)
      else pt
    val tvars = if (owningTree.isEmpty) Nil else newTypeVars(added)
    state.constraint = state.constraint.add(added, tvars)
    (added, tvars)
  }

  /**  Same as `constrained(pt, EmptyTree)`, but returns just the created polytype */
  def constrained(pt: PolyType)(implicit ctx: Context): PolyType = constrained(pt, EmptyTree)._1

  /** Interpolate those undetermined type variables in the widened type of this tree
   *  which are introduced by type application contained in the tree.
   *  If such a variable appears covariantly in type `tp` or does not appear at all,
   *  approximate it by its lower bound. Otherwise, if it appears contravariantly
   *  in type `tp` approximate it by its upper bound.
   */
  def interpolateUndetVars(tree: Tree)(implicit ctx: Context): Unit = Stats.track("interpolateUndetVars") {
    val tp = tree.tpe.widen
    val constraint = ctx.typerState.constraint

    println(s"interpolate undet vars in ${tp.show}, pos = ${tree.pos}, mode = ${ctx.mode}, undets = ${constraint.uninstVars map (tvar => s"${tvar.show}@${tvar.owningTree.pos}")}")
    println(s"qualifying undet vars: ${constraint.uninstVars filter qualifies map (_.show)}")
    println(s"fulltype: $tp") // !!! DEBUG
    println(s"constraint: ${constraint.show}")

    def qualifies(tvar: TypeVar) = tree contains tvar.owningTree
    val vs = tp.variances(tvar => (constraint contains tvar) && qualifies(tvar))
    println(s"variances = $vs")
    var changed = false
    vs foreachBinding { (tvar, v) =>
      if (v != 0) {
        println(s"interpolate ${if (v == 1) "co" else "contra"}variant ${tvar.show} in ${tp.show}")
        tvar.instantiate(fromBelow = v == 1)
        changed = true
      }
    }
    if (changed) // instantiations might have uncovered new typevars to interpolate
      interpolateUndetVars(tree)
    else
      constraint.foreachUninstVar { tvar =>
        if (!(vs contains tvar) && qualifies(tvar)) {
          println(s"instantiating non-occurring $tvar in $tp")
          tvar.instantiate(fromBelow = true)
        }
      }
  }

  /** Instantiate undetermined type variables to that type `tp` is
   *  maximized and return None. If this is not possible, because a non-variant
   *  typevar is not uniquely determined, return that typevar in a Some.
   */
  def maximizeType(tp: Type)(implicit ctx: Context): Option[TypeVar] = Stats.track("maximizeType") {
    val constraint = ctx.typerState.constraint
    val vs = tp.variances(constraint contains _)
    var result: Option[TypeVar] = None
    vs foreachBinding { (tvar, v) =>
      if (v == 1) tvar.instantiate(fromBelow = false)
      else if (v == -1) tvar.instantiate(fromBelow = true)
      else {
        val bounds = ctx.typerState.constraint.bounds(tvar.origin)
        if (!(bounds.hi <:< bounds.lo)) result = Some(tvar)
        tvar.instantiate(fromBelow = false)
      }
    }
    result
  }
}

/* not needed right now

  def isSubTypes(actuals: List[Type], formals: List[Type])(implicit ctx: Context): Boolean = formals match {
    case formal :: formals1 =>
      actuals match {
        case actual :: actuals1 => actual <:< formal && isSubTypes(actuals1, formals1)
        case _ => false
      }
    case nil =>
      actuals.isEmpty
  }

    def formalParameters[T](mtp: MethodType, actuals: List[T])(isRepeated: T => Boolean)(implicit ctx: Context) =
      if (mtp.isVarArgs && !(actuals.nonEmpty && isRepeated(actuals.last))) {
        val leading = mtp.paramTypes.init
        val repeated = mtp.paramTypes.last.typeArgs.head
        val trailing = List.fill(actuals.length - leading.length)(repeated)
        leading ++ trailing
      }
      else mtp.paramTypes
  */