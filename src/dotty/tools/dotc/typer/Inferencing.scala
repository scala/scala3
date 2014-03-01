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
      case _: ValueTypeOrProto if !(pt isRef defn.UnitClass) =>
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

  object NoViewsAllowed extends Compatibility {
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean = false
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
        def qualifies(m: SingleDenotation) = compat.normalizedCompatible(m.info, memberProto)
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

    override def computeHash = addDelta(doHash(name, memberProto), if (compat == NoViewsAllowed) 1 else 0)
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
      case tp: ProtoType => SelectionProto(name, WildcardType, typer)
      case _ => SelectionProto(name, tp, typer)
    }

  /** A prototype for expressions [] that are in some unspecified selection operation
   *
   *    [].?: ?
   *
   *  Used to indicate that expression is in a context where the only valid
   *  operation is further selection. In this case, the expression need not be a value.
   *  @see checkValue
   */
  object AnySelectionProto extends SelectionProto(nme.WILDCARD, WildcardType, NoViewsAllowed)

  /** A prototype for selections in pattern constructors */
  class UnapplySelectionProto(name: Name) extends SelectionProto(name, WildcardType, NoViewsAllowed)

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

    def derivedFunProto(args: List[untpd.Tree], resultType: Type, typer: Typer) =
      if ((args eq this.args) && (resultType eq this.resultType) && (typer eq this.typer)) this
      else new FunProto(args, resultType, typer)

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

    def map(tm: TypeMap)(implicit ctx: Context): FunProto =
      derivedFunProto(args, tm(resultType), typer)

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T = ta(x, resultType)
  }

  /** A prototype for implicitly inferred views:
   *
   *    []: argType => resultType
   */
  abstract case class ViewProto(argType: Type, override val resultType: Type)(implicit ctx: Context)
  extends CachedGroundType with ApplyingProto {
    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean = /*ctx.conditionalTraceIndented(lookingForInfo, i"?.info isMatchedBy $tp ${tp.getClass}")*/ {
  	  ctx.typer.isApplicable(tp, argType :: Nil, resultType)
    }

    def derivedViewProto(argType: Type, resultType: Type)(implicit ctx: Context) =
      if ((argType eq this.argType) && (resultType eq this.resultType)) this
      else ViewProto(argType, resultType)

    def map(tm: TypeMap)(implicit ctx: Context): ViewProto = derivedViewProto(tm(argType), tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T = ta(ta(x, argType), resultType)

    override def namedPartsWith(p: NamedType => Boolean)(implicit ctx: Context): collection.Set[NamedType] =
      AndType.unchecked(argType, resultType).namedPartsWith(p) // this is more efficient than oring two namedParts sets
  }

  class CachedViewProto(argType: Type, resultType: Type)(implicit ctx: Context) extends ViewProto(argType, resultType) {
    override def computeHash = doHash(argType, resultType)
  }

  object ViewProto {
    def apply(argType: Type, resultType: Type)(implicit ctx: Context) =
      unique(new CachedViewProto(argType, resultType))
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

    def derivedPolyProto(targs: List[Type], resultType: Type) =
      if ((targs eq this.targs) && (resultType eq this.resultType)) this
      else PolyProto(targs, resultType)

    def map(tm: TypeMap)(implicit ctx: Context): PolyProto =
      derivedPolyProto(targs mapConserve tm, tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta.foldOver(x, targs), resultType)
  }

  /** A prototype for expressions [] that are known to be functions:
   *
   *    [] _
   */
  object AnyFunctionProto extends UncachedGroundType with ProtoType {
    def isMatchedBy(tp: Type)(implicit ctx: Context) = true
    def map(tm: TypeMap)(implicit ctx: Context) = this
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context) = x
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
      case poly: PolyType => normalize(constrained(poly).resultType, pt)
      case mt: MethodType if !mt.isDependent /*&& !pt.isInstanceOf[ApplyingProto]*/ =>
        if (mt.isImplicit) mt.resultType
        else {
          val rt = normalize(mt.resultType, pt)
          if (pt.isInstanceOf[ApplyingProto])
            mt.derivedMethodType(mt.paramNames, mt.paramTypes, rt)
          else {
            val ft = defn.FunctionType(mt.paramTypes, rt)
            if (mt.paramTypes.nonEmpty || ft <:< pt) ft else rt
          }
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
    val result = new IsFullyDefinedAccumulator(force)(nestedCtx).process(tp)
    if (result) nestedCtx.typerState.commit()
    result
  }

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, pos: Position)(implicit ctx: Context) =
    if (isFullyDefined(tp, ForceDegree.all)) tp
    else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $pos") // !!! DEBUG

  /** The accumulator which forces type variables using the policy encoded in `force`
   *  and returns whether the type is fully defined. Two phases:
   *  1st Phase: Try to stantiate covariant and non-variant type variables to
   *  their lower bound. Record whether succesful.
   *  2nd Phase: If first phase was succesful, instantiate all remaining type variables
   *  to their upper bound.
   */
  private class IsFullyDefinedAccumulator(force: ForceDegree.Value)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Type = {
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      inst
    }
    private var toMaximize: Boolean = false
    def apply(x: Boolean, tp: Type): Boolean = tp.dealias match {
      case _: WildcardType =>
        false
      case tvar: TypeVar if !tvar.isInstantiated =>
        if (force == ForceDegree.none) false
        else {
          val minimize =
            variance >= 0 && !(
              force == ForceDegree.noBottom &&
              isBottomType(ctx.typeComparer.approximation(tvar.origin, fromBelow = true)))
          if (minimize) instantiate(tvar, fromBelow = true)
          else toMaximize = true
          foldOver(x, tvar)
        }
      case tp =>
        foldOver(x, tp)
    }

    private class UpperInstantiator(implicit ctx: Context) extends TypeAccumulator[Unit] {
      def apply(x: Unit, tp: Type): Unit = {
        tp match {
          case tvar: TypeVar if !tvar.isInstantiated =>
            instantiate(tvar, fromBelow = false)
          case _ =>
        }
        foldOver(x, tp)
      }
    }

    def process(tp: Type): Boolean = {
      val res = apply(true, tp)
      if (res && toMaximize) new UpperInstantiator().apply((), tp)
      res
    }
  }

  def isBottomType(tp: Type)(implicit ctx: Context) =
    tp == defn.NothingType || tp == defn.NullType

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

  /** Check that `tp` is a class type with a stable prefix. Also, if `isFirst` is
   *  false check that `tp` is a trait.
   *  @return  `tp` itself if it is a class or trait ref, ObjectClass.typeRef if not.
   */
  def checkClassTypeWithStablePrefix(tp: Type, pos: Position, traitReq: Boolean)(implicit ctx: Context): Type =
    tp.underlyingClassRef match {
      case tref: TypeRef =>
        checkStable(tref.prefix, pos)
        if (traitReq && !(tref.symbol is Trait)) ctx.error(i"$tref is not a trait", pos)
        tp
    case _ =>
      ctx.error(i"$tp is not a class type", pos)
      defn.ObjectClass.typeRef
  }

  /** Check that (return) type of implicit definition is not empty */
  def checkImplicitTptNonEmpty(defTree: untpd.ValOrDefDef)(implicit ctx: Context): Unit = defTree.tpt match {
    case TypeTree(original) if original.isEmpty =>
      val resStr = if (defTree.isInstanceOf[untpd.DefDef]) "result " else ""
      ctx.error(i"${resStr}type of implicit definition needs to be given explicitly", defTree.pos)
    case _ =>
  }

  /** Check that a non-implicit parameter making up the first parameter section of an
   *  implicit conversion is not a singleton type.
   */
  def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = vparamss match {
    case (vparam :: Nil) :: _ if !(vparam.symbol is Implicit) =>
      if (vparam.tpt.tpe.isInstanceOf[SingletonType])
        ctx.error(s"implicit conversion may not have a parameter of singleton type", vparam.tpt.pos)
    case _ =>
  }

  /** Ensure that the first type in a list of parent types Ps points to a non-trait class.
   *  If that's not already the case, add one. The added class type CT is determined as follows.
   *  First, let C be the unique class such that
   *  - there is a parent P_i such that P_i derives from C, and
   *  - for every class D: If some parent P_j, j <= i derives from D, then C derives from D.
   *  Then, let CT be the smallest type which
   *  - has C as its class symbol, and
   *  - for all parents P_i: If P_i derives from C then P_i <:< CT.
   */
  def ensureFirstIsClass(parents: List[Type])(implicit ctx: Context): List[Type] = {
    def realClassParent(cls: Symbol): ClassSymbol =
      if (!cls.isClass) defn.ObjectClass
      else if (!(cls is Trait)) cls.asClass
      else cls.asClass.classParents match {
        case parentRef :: _ => realClassParent(parentRef.symbol)
        case nil => defn.ObjectClass
      }
    def improve(candidate: ClassSymbol, parent: Type): ClassSymbol = {
      val pcls = realClassParent(parent.classSymbol)
      if (pcls derivesFrom candidate) pcls else candidate
    }
    parents match {
      case p :: _ if p.classSymbol.isRealClass => parents
      case _ =>
        val pcls = (defn.ObjectClass /: parents)(improve)
        typr.println(i"ensure first is class $parents%, % --> ${parents map (_ baseTypeWithArgs pcls)}%, %")
        val ptype = ctx.typeComparer.glb(
            defn.ObjectType :: (parents map (_ baseTypeWithArgs pcls)))
        ptype :: parents
    }
  }

  /** Ensure that first parent tree refers to a real class. */
  def ensureFirstIsClass(parents: List[Tree], pos: Position)(implicit ctx: Context): List[Tree] = parents match {
    case p :: ps if p.tpe.classSymbol.isRealClass => parents
    case _ =>
      // add synthetic class type
      val first :: _ = ensureFirstIsClass(parents.tpes)
      TypeTree(checkFeasible(first, pos, i"\n in inferred parent $first")).withPos(pos) :: parents
  }

  /** Check that any top-level type arguments in this type are feasible, i.e. that
   *  their lower bound conforms to their upper cound. If a type argument is
   *  infeasible, issue and error and continue with upper bound.
   */
  def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp match {
    case tp: RefinedType =>
      tp.derivedRefinedType(tp.parent, tp.refinedName, checkFeasible(tp.refinedInfo, pos, where))
    case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
      ctx.error(i"no type exists between low bound $lo and high bound $hi$where", pos)
      tp.derivedTypeAlias(hi)
    case _ =>
      tp
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
          def doubleDefError(decl: Symbol, other: Symbol): Unit = {
            def ofType = if (decl.isType) "" else i": ${other.info}"
            def explanation =
              if (!decl.isSourceMethod) ""
              else "\n (both definitions have the same erased type signature)"
            ctx.error(i"$decl is already defined as $other$ofType$explanation", decl.pos)
          }
          if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if ((decl is HasDefaultParams) && (other is HasDefaultParams)) {
          ctx.error(i"two or more overloaded variants of $decl have default arguments")
          decl resetFlag HasDefaultParams
        }
      }
      seen(decl.name) = decl :: seen(decl.name)
    }
  }

  def checkInstantiatable(cls: ClassSymbol, pos: Position): Unit = {
    ??? // to be done in later phase: check that class `cls` is legal in a new.
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
    case tp: TypeBounds if tp.lo eq tp.hi => // default case, inlined for speed
      tp.derivedTypeAlias(wildApprox(tp.lo, theMap))
    case PolyParam(pt, pnum) =>
      WildcardType(wildApprox(pt.paramBounds(pnum)).bounds)
    case MethodParam(mt, pnum) =>
      WildcardType(TypeBounds.upper(wildApprox(mt.paramTypes(pnum))))
    case tp: TypeVar =>
      val inst = tp.instanceOpt
      if (inst.exists) wildApprox(inst)
      else ctx.typerState.constraint.at(tp.origin) match {
        case bounds: TypeBounds => wildApprox(WildcardType(bounds))
        case NoType => WildcardType
      }
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
    case tp: SelectionProto =>
      tp.derivedSelectionProto(tp.name, wildApprox(tp.memberProto), NoViewsAllowed)
    case tp: ViewProto =>
      tp.derivedViewProto(wildApprox(tp.argType), wildApprox(tp.resultType))
    case  _: ThisType | _: BoundType | NoPrefix => // default case, inlined for speed
      tp
    case _ =>
      (if (theMap != null) theMap else new WildApproxMap).mapOver(tp)
  }

  private[Inferencing] class WildApproxMap(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = wildApprox(tp, this)
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
  def interpolateUndetVars(tree: Tree)(implicit ctx: Context): Unit = {
    val constraint = ctx.typerState.constraint
    val qualifies = (tvar: TypeVar) => tree contains tvar.owningTree
    def interpolate() = Stats.track("interpolateUndetVars") {
      val tp = tree.tpe.widen
      constr.println(s"interpolate undet vars in ${tp.show}, pos = ${tree.pos}, mode = ${ctx.mode}, undets = ${constraint.uninstVars map (tvar => s"${tvar.show}@${tvar.owningTree.pos}")}")
      constr.println(s"qualifying undet vars: ${constraint.uninstVars filter qualifies map (tvar => s"$tvar / ${tvar.show}")}, constraint: ${constraint.show}")

      val vs = tp.variances(qualifies)
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
        for (tvar <- constraint.uninstVars)
          if (!(vs contains tvar) && qualifies(tvar)) {
            typr.println(s"instantiating non-occurring ${tvar.show} in ${tp.show}")
            tvar.instantiate(fromBelow = true)
          }
    }
    if (constraint.uninstVars exists qualifies) interpolate()
  }

  /** Instantiate undetermined type variables to that type `tp` is
   *  maximized and return None. If this is not possible, because a non-variant
   *  typevar is not uniquely determined, return that typevar in a Some.
   */
  def maximizeType(tp: Type)(implicit ctx: Context): Option[TypeVar] = Stats.track("maximizeType") {
    val vs = tp.variances(alwaysTrue)
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