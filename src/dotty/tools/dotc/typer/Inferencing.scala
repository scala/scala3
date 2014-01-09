package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import Trees._
import Constants._
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import util.common._
import Decorators._
import ErrorReporting.{errorType, InfoString}
import config.Printers._
import collection.mutable

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
    def isCompatible(tp: Type, pt: Type)(implicit ctx: Context): Boolean =
      tp.widenExpr <:< pt.widenExpr || viewExists(tp, pt)

    /** Test compatibility after normalization in a fresh typerstate. */
    def normalizedCompatible(tp: Type, pt: Type)(implicit ctx: Context) = {
      val nestedCtx = ctx.fresh.withExploreTyperState
      isCompatible(normalize(tp, pt)(nestedCtx), pt)(nestedCtx)
    }

    /** Check that the result type of the current method
     *  fits the given expected result type.
     */
    def constrainResult(mt: Type, pt: Type)(implicit ctx: Context): Boolean = pt match {
      case FunProto(_, result, _) =>
        mt match {
          case mt: MethodType =>
            mt.isDependent || constrainResult(mt.resultType, pt.resultType)
          case _ =>
            true
        }
      case pt: ValueType if !(pt isRef defn.UnitClass) =>
        mt match {
          case mt: MethodType =>
            mt.isDependent || isCompatible(normalize(mt, pt), pt)
          case _ =>
            isCompatible(mt, pt)
        }
      case _ =>
        true
    }
  }

  /** A prototype for expressions [] that are part of a selection operation:
   *
   *       [ ].name: proto
   */
  class SelectionProto(val name: Name, proto: Type)
  extends RefinedType(WildcardType, name)(_ => proto) with ProtoType with Compatibility {
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean = false
    override def isMatchedBy(tp1: Type)(implicit ctx: Context) =
      name == nme.WILDCARD || {
        val mbr = tp1.member(name)
        mbr.exists && mbr.hasAltWith(m => normalizedCompatible(m.info, proto))
      }
    override def toString = "Proto" + super.toString
    override def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(implicit ctx: Context): RefinedType = {
      val tp1 @ RefinedType(parent1, refinedName1) = super.derivedRefinedType(parent, refinedName, refinedInfo)
      if (tp1 eq this) this
      else {
        assert(parent == WildcardType)
        new SelectionProto(refinedName1, tp1.refinedInfo)
      }
    }
  }

  /** Create a selection proto-type, but only one level deep;
   *  treat constructors specially
   */
  def selectionProto(name: Name, tp: Type) =
    if (name.isConstructorName) WildcardType
    else tp match {
      case tp: UnapplyFunProto => new UnapplySelectionProto(name)
      case tp: ProtoType => new SelectionProto(name, WildcardType)
      case _ => new SelectionProto(name, tp)
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

  /** A prototype for selections in pattern constructors */
  class UnapplySelectionProto(name: Name) extends SelectionProto(name, WildcardType)

  trait ApplyingProto extends ProtoType

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType
   */
  case class FunProto(args: List[untpd.Tree], override val resultType: Type, typer: Typer)(implicit ctx: Context)
  extends UncachedGroundType with ApplyingProto {
    private var myTypedArgs: List[Tree] = Nil

    /** A map in which typed arguments can be stored to be later integrated in `typedArgs`. */
    private var myTypedArg: SimpleMap[untpd.Tree, Tree] = SimpleMap.Empty

    def isMatchedBy(tp: Type)(implicit ctx: Context) =
      typer.isApplicable(tp, Nil, typedArgs, resultType)

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
     *  used to avoid repeated typings of trees when backtracking.
     */
    def typedArg(arg: untpd.Tree, formal: Type)(implicit ctx: Context): Tree = {
      var targ = myTypedArg(arg)
      if (targ == null) {
        val counts = ctx.reporter.errorCounts
        targ = typer.typedUnadapted(arg, formal)
        if (ctx.reporter.wasSilent(counts))
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
  extends CachedGroundType with ApplyingProto {
 //   def lookingForInfo = resultType match {
 //     case rt: SelectionProto => rt.name.toString == "info"
 //     case _ => false
 //   }
    def isMatchedBy(tp: Type)(implicit ctx: Context) = /*ctx.conditionalTraceIndented(lookingForInfo, i"?.info isMatchedBy $tp ${tp.getClass}")*/ {
      ctx.typer.isApplicable(tp, argType :: Nil, resultType)
    }
    override def namedPartsWith(p: NamedType => Boolean)(implicit ctx: Context): collection.Set[NamedType] =
      AndType.unchecked(argType, resultType).namedPartsWith(p) // this is more efficient than oring two namedParts sets
    override def computeHash = doHash(argType, resultType)
  }

  class UnapplyFunProto(typer: Typer)(implicit ctx: Context) extends FunProto(
      untpd.TypedSplice(dummyTreeOfType(WildcardType)) :: Nil, WildcardType, typer)

  /** A prototype for expressions [] that are type-parameterized:
   *
   *    [] [targs] resultType
   */
  case class PolyProto(targs: List[Type], override val resultType: Type) extends UncachedGroundType with ProtoType {
    override def isMatchedBy(tp: Type)(implicit ctx: Context) = {
      def isInstantiatable(tp: Type) = tp.widen match {
        case PolyType(paramNames) => paramNames.length == targs.length
        case _ => false
      }
      isInstantiatable(tp) || tp.member(nme.apply).hasAltWith(d => isInstantiatable(d.info))
    }
  }

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
      case pt: PolyType => normalize(constrained(pt).resultType, pt)
      case mt: MethodType if !mt.isDependent /*&& !pt.isInstanceOf[ApplyingProto]*/ =>
        if (mt.isImplicit) mt.resultType
        else {
          val rt = normalize(mt.resultType, pt)
          val ft = defn.FunctionType(mt.paramTypes, rt)
          if (mt.paramTypes.nonEmpty || ft <:< pt) ft else rt
        }
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
    private var vs: SimpleMap[TypeVar, Integer] = null
    private var rootTp: Type = null
    def isContravariant(tvar: TypeVar) = {
      (variance < 0) && { // otherwise no need to compute, it can't be contravariant
        if (vs == null) vs = rootTp.variances(alwaysTrue)
        vs(tvar) < 0
      }
    }
    def traverse(tp: Type): Boolean = {
      rootTp = tp
      apply(true, tp)
    }
    def apply(x: Boolean, tp: Type) = !x || isOK(tp) && foldOver(x, tp)
    def isOK(tp: Type): Boolean = tp match {
      case _: WildcardType =>
        false
      case tvar: TypeVar if !tvar.isInstantiated =>
        def isBottomType(tp: Type) = tp == defn.NothingType || tp == defn.NullType
        force != ForceDegree.none && {
          val forceUp =
            isContravariant(tvar) ||
              force == ForceDegree.noBottom &&
              isBottomType(ctx.typeComparer.approximation(tvar.origin, fromBelow = true))
          val inst = tvar.instantiate(fromBelow = !forceUp)
          typr.println(i"forced instantiation of ${tvar.origin} = $inst")
          traverse(inst)
        }
      case _ =>
        true
    }
  }

  /** Recursively widen and also follow type declarations and type aliases. */
  def widenForMatchSelector(tp: Type)(implicit ctx: Context): Type = tp.widen match {
    case tp: TypeRef if !tp.symbol.isClass => widenForMatchSelector(tp.info.bounds.hi)
    case tp => tp
  }

  /** Following type aliases and stripping refinements and annotations, if one arrives at a
   *  class type reference where the class has a companion module, a reference to
   *  that companion module. Otherwise NoType
   */
  def companionRef(tp: Type)(implicit ctx: Context): Type = tp.underlyingClassRef match {
    case tp: TypeRef =>
      val companion = tp.classSymbol.companionModule
      if (companion.exists)
        companion.valRef.asSeenFrom(tp.prefix, companion.symbol.owner)
      else NoType
    case _ => NoType
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
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(i"Prefix of type ${tp.widenIfUnstable} is not stable", pos)

  /** Check that `tp` is a class type with a stable prefix.
   *  @return  Underlying class type if type checks out OK, ObjectClass.typeRef if not.
   */
  def checkClassTypeWithStablePrefix(tp: Type, pos: Position)(implicit ctx: Context): TypeRef =
    tp.underlyingClassRef match {
      case tp: TypeRef =>
        checkStable(tp.prefix, pos)
        tp
    case _ =>
      ctx.error(s"$tp is not a class type", pos)
      defn.ObjectClass.typeRef
  }

  /** Check that class does not define */
  def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = {
    val seen = new mutable.HashMap[Name, List[Symbol]] {
      override def default(key: Name) = Nil
    }
    typr.println(i"check no double defs $cls")
    for (decl <- cls.info.decls) {
      for (other <- seen(decl.name)) {
        typr.println(i"conflict? $decl $other")
        if (decl.signature matches other.signature) {
          val ofType = if (decl.isType) "" else i": ${other.info}"
          val explanation =
            if (!decl.isSourceMethod) ""
            else "\n (both definitions have the same erased type signature)"
          ctx.error(i"$decl is already defined as $other$ofType$explanation", decl.pos)
        }
      }
      seen(decl.name) = decl :: seen(decl.name)
    }
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

    /* !!! DEBUG
    println(s"interpolate undet vars in ${tp.show}, pos = ${tree.pos}, mode = ${ctx.mode}, undets = ${constraint.uninstVars map (tvar => s"${tvar.show}@${tvar.owningTree.pos}")}")
    println(s"qualifying undet vars: ${constraint.uninstVars filter qualifies map (_.show)}")
    println(s"fulltype: $tp") // !!! DEBUG
    println(s"constraint: ${constraint.show}")
    */

    def qualifies(tvar: TypeVar) = tree contains tvar.owningTree
    val vs = tp.variances(tvar => (constraint contains tvar) && qualifies(tvar))
    var changed = false
    vs foreachBinding { (tvar, v) =>
      if (v != 0) {
        typr.println(s"interpolate ${if (v == 1) "co" else "contra"}variant ${tvar.show} in ${tp.show}")
        tvar.instantiate(fromBelow = v == 1)
        changed = true
      }
    }
    if (changed) // instantiations might have uncovered new typevars to interpolate
      interpolateUndetVars(tree)
    else
      constraint.foreachUninstVar { tvar =>
        if (!(vs contains tvar) && qualifies(tvar)) {
          typr.println(s"instantiating non-occurring $tvar in $tp")
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

  private lazy val dummyTree = untpd.Literal(Constant(null))

  /** Dummy tree to be used as an argument of a FunProto or ViewProto type */
  def dummyTreeOfType(tp: Type): Tree = dummyTree withTypeUnchecked tp
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